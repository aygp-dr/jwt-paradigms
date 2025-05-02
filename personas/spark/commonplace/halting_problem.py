def halts(program, input):
    """Determine if program will halt when run with input."""
    # This is the function we are proving cannot exist
    ...

def paradox(program):
    """Create a paradoxical situation if halts() exists."""
    if halts(program, program):
        # If program would halt when run on itself,
        # then loop forever
        while True:
            pass
    else:
        # If program would loop forever when run on itself,
        # then halt immediately
        return

# The paradox
paradox(paradox)
