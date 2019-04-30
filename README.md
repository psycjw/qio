# qio

An updated version of the QIO Haskell Monad, written by Dr. Alexander S Green, and Prof. Thorsten Altenkirch, found at https://github.com/alexandersgreen/qio-haskell and http://hackage.haskell.org/package/QIO. 

This project is being created for my Dissertation, and has aims to implement the following:
- Update current QIO project
- Implement Grover's Algorithm
- Look at improving the current implementation of Shor's Algorithm
- Implement a Quantum Circuit Builder/Interpreter

## Grover's algorithm
There are 2 differing implementations of Grover's algorithm, namely Grover.hs and Grover2.hs.

### Grover.hs
Grover.hs is based on the typical implementation of Grover's algorithm, as described in the book "Quantum Computer Science: An Introduction" by "N. David Mermin" 2007. It converts an inputted boolean function into an oracle, and then diffuses using hadamard gates, not gates and a controlled Z gate.

The boolean function is inputted in a form such that the variables in a line are AND'd together, and each line is OR'd together.
E.g. the function
(x1 & -x2) | x1 | x2
is inputted as
1 -2
1
2

Input qubits are then created and put in the state |+>, each representing the variables in the function. These are then used in Controlled-X gates for each line, where NOT'd variables are NOT'd and unNOT'd around each Controlled-X gate.
E.g. for the above function (x1 & -x2) | x1 | x2, the oracle is:
NOT x2
CONDX x1 x2 o1
NOT x2
CONDX x1 o2
CONDX x2 o3

After the oracle is applied, the input qubits are diffused using Hadamard gates, followed by NOT gates, on each. Then a Controlled-Z gate is applied, where the last input qubit has the Pauli-Z gate applied to it if the other input qubits are measured to be in the state |1>. The NOT and Hadamard gates are then applied again, and the qubits will have been diffused.

This is repeated for x iterations, where x = (pi/4)(N/s)^(1/2), N = 2^n, n = amount of variables in function, s = amount of solutions to function, returning each solution found on separate lines. Each set found has a probability of >50% of being a solution.

## Quantum Circuit Builder
The quantum circuit builder can be run using the main function in Circuit/Main.hs. This will open up a window containing a row of buttons, and multiple rows of qubits, all of which start in the state |0>. Qubits can be added or removed using the + and - buttons in the top left of the window. Gates can be put onto the circuit board by clicking the appropriate button on the top, and then clicking the position to place it on the board. When a gate has been picked up it will follow the mouse cursor, and the right mouse button can be pressed to clear it. Gates already on the board can be picked up and moved around by clicking on them, and the circuit board can be cleared using the Clear button.

All of the gates (apart from the Swap gate) can be made conditional, by pressing the number keys representing the qubits they depend on.
E.g. to apply a Hadamard gate to the 3rd qubit (i.e. qubit q2) depending on the first two qubits (i.e. qubits q0 and q1), then the Hadamard button should first be clicked, and then the number keys 0 and 1 should be pressed, followed by placing the gate on the 3rd qubit line (i.e. line for qubit q2). 

The Swap gate is used in a similar way, where the qubit to swap with is defined by pressed the number key representing it.

The Run button can be used to run the quantum circuit, where all the qubits will be measured after applying the gates on the circuit to them, and a measurement set will be shown. This can also be done using the Enter key.

The Sim button can be used to display the probability distributions for all measurements from the quantum circuit.
