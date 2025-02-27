import subprocess as sp
import tempfile as tf
import os
import concurrent.futures
import shutil

def get_sample_output(src):
    result = sp.run(['java', '-jar', 'wacc-reference-cli.jar', src, '-x'], capture_output=True, text=True)
    
    if result.stderr:
        raise CompilationError(result.stderr)

    if "subprocess-shutdown-hook-monitor" in result.stdout:
        # bruh our shitty reference compiler is complaining again
        return get_sample_output(src)


    return result.stdout[:-1] # filter out the last trailing \n

def gen(src):
    actual_output = get_sample_output(src)
    dst = src.replace("testsuite/test_cases", "testsuite/expected")
    os.makedirs(os.path.dirname(dst), exist_ok=True)
    with open(dst, "w") as f:
        f.write(actual_output)
    print(f"{dst} has been produced")
    return True
        
def main():
    test_dir = "testsuite/test_cases"
    expected_output_dir = "testsuite/expected"
    test_files = []

    if os.path.exists(expected_output_dir):
        shutil.rmtree(expected_output_dir)
        
    for root, _, files in os.walk(test_dir):
        for file in files:
            if file.endswith(".wacc"):
                test_files.append(os.path.join(root, file))

    with concurrent.futures.ProcessPoolExecutor() as executor:
        results = list(executor.map(gen, test_files))
    print("done")

if __name__ == "__main__":
    main()
