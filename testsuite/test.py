import subprocess as sp
import tempfile as tf
import os
import concurrent.futures

class CompilationError(Exception):
    pass

def compile(src, dst):
    result = sp.run(['./target/debug/compiler', '--codegen', src], capture_output=True, text=True)
    
    if result.stderr:
        raise CompilationError(result.stderr)
        
    with open(dst, 'w') as f:
        f.write(result.stdout)

def link(src, dst):
    result = sp.run(['gcc', '-o', dst, src], capture_output=True, text=True)
    
    if result.stderr:
        raise CompilationError(result.stderr)
    
def compile_and_link(src, dst):
    with tf.NamedTemporaryFile(mode='w+t', suffix='.S', delete=True) as f:
        compile(src, f.name)
        link(f.name, dst)

def emulate(exe):
    result = sp.run(['qemu-x86_64', exe], capture_output=True, text=True)
    
    if result.stderr:
        raise Exception(result.stderr)
    
    return result.stdout

def get_sample_output(src):
    result = sp.run(['java', '-jar', 'wacc-reference-cli.jar', src, '-x'], capture_output=True, text=True)
    
    if result.stderr:
        raise CompilationError(result.stderr)

    if "subprocess-shutdown-hook-monitor" in result.stdout:
        # bruh our shitty reference compiler is complaining again
        return get_sample_output(src)


    return result.stdout[:-1] # filter out the last trailing \n

def run_and_compare(src):
    sample_output = get_sample_output(src)
    try:
        with tf.NamedTemporaryFile(mode='w+b', delete=True) as f:
            compile_and_link(src, f.name)
            actual_output = emulate(f.name)
    except CompilationError as e:
        actual_output = str(e)
    
    if sample_output == actual_output:
        print(f"{src} pass")
        return True
    else:
        print(f"{src} INCORRECT")
        print(f"Expected: {sample_output}\nActual: {actual_output}")
        return False
        
def main():
    test_dir = "testsuite/test_cases"
    test_files = []
    for root, _, files in os.walk(test_dir):
        for file in files:
            if file.endswith(".wacc"):
                test_files.append(os.path.join(root, file))

    with concurrent.futures.ProcessPoolExecutor() as executor:
        results = list(executor.map(run_and_compare, test_files))

    total_tests = len(results)
    passed_tests = sum(results)
    print(f"\nTotal tests: {total_tests}, Passed: {passed_tests}, Failed: {total_tests - passed_tests}")

if __name__ == "__main__":
    main()
