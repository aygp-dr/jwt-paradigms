def process_sequence(sequence):
    for item in sequence:
        print(item)
        
# Works with any iterable object, regardless of its specific type
process_sequence([1, 2, 3])           # List
process_sequence((4, 5, 6))           # Tuple
process_sequence({7, 8, 9})           # Set
process_sequence("hello")             # String
process_sequence(range(5))            # Range
