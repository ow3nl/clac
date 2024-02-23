# functional-clac
A clac interpreter built in Haskell. Accepts flag -trace and can read clac files.

clac is an esoteric postfix language, which adds integers to a stack and reads tokens from a queue.
The state of the stack and queue can be printed on each operation using the -trace flag.
Valid clac token includes:
- Any valid integer
- Standard operators (+, -, x, /, %, **)
- "<"        clears the top 2 integers from the stack and replaces them with 1 if the top element is greater than the second top (replaces with 0 otherwise)
- "drop"     removes the top element on the stack
- "swap"     swaps the top 2 elements on the stack
- "rot"      rotates the top 3 elements on the stack
- "pick"     if n is on top of the stack, replaces n with the nth token down the stack (excluding n itself)
- "print"    prints the top element of the stack and removes it
- "if"       skips the next three tokens if the number on top of the stack is 0
- "skip"     if n is on top of the stack, removes n and skips the next n tokens in the queue
- "quit"     exits the program
