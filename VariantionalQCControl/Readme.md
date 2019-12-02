Control-advantage.py demonstrates the advantage of parameterized circuits with control over parameterized circuits without control.

Both circuits aim to learn to classify all 4 bit inputs according to the ground truth labelling function:
lfunc: [1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1]

# learn_without_control:
## Arguments: 
* wires: number of qubits
* lfunc: labelling function to learn
* epochs: epochs of training
* step_size: step size used for gradient descent
* verbose: flag that marks the degree of outputs while training
* seed: Random seed if it is to be fixed, None by default
## Function:
Constructs a circuit that applies X,Y,Z to every qubit, first with parameters theta and then with parameters Arho. The predicted label is measured in the 4th qubit. We minimize the average negative log likelihood using gradient descent with analytically calculated gradients. The function returns the value of the loss function at each epoch.

# learn_with_control:
## Arguments: 
* wires: number of qubits
* lfunc: labelling function to learn
* epochs: epochs of training
* step_size: step size used for gradient descent
* verbose: flag that marks the degree of outputs while training
* seed: Random seed if it is to be fixed, None by default
## Function:
Constructs a circuit that applies X,Y,Z to every qubit, first with parameters theta, measures the first qubit, and if the outcome is 0, applies it with parameters Arho, and if the outcome is 1, applies it with parameters Brho. The predicted label is measured in the 4th qubit. We minimize the average negative log likelihood using gradient descent with analytically calculated gradients. The function returns the value of the loss function at each epoch.

# Main function:

The main function sets hyperparameters and plots the losses while training with control and without control.