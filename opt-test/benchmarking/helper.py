def generate_long_expression(n, operation='+'):
    if operation == '+':
        return " + ".join(str(i) for i in range(1, n+1))
    elif operation == '-':
        return " - ".join(str(i) for i in range(1, n+1))
    elif operation == '*':
        return " * ".join(str(i) for i in range(1, n+1))

# Generate expression for x (addition up to 1000)
x_expression = generate_long_expression(1000, '+')
print(f"int x = {x_expression};")

# Generate expression for y (subtraction up to 1000)
y_expression = generate_long_expression(1000, '-')
print(f"int y = -{y_expression};")
