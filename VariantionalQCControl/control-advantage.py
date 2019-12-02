import numpy as np
import matplotlib.pyplot as plt

PX = np.matrix([[0, 1],[1, 0]])
PY = np.matrix([[0, -1j],[1j, 0]])
PZ = np.matrix([[1, 0], [0, -1]])
Id = np.matrix([[1, 0], [0, 1]])

Pauli = [PX, PY, PZ, Id]

def expX(theta):
    eigplus = (1./np.sqrt(2))*np.matrix([[1, 1]])
    eigminus = (1./np.sqrt(2))*np.matrix([[1, -1]])
    return np.exp(-1j*theta/2)*np.matmul(eigplus.H,eigplus) + np.exp(1j*theta/2)*np.matmul(eigminus.H,eigminus)

def expZ(theta):
    eigplus = np.matrix([[1, 0]])
    eigminus = np.matrix([[0, -1]])
    return np.exp(-1j*theta/2)*np.matmul(eigplus.H,eigplus) + np.exp(1j*theta/2)*np.matmul(eigminus.H,eigminus)

def expY(theta):
    eigplus = (1./np.sqrt(2))*np.matrix([[1, 1j]])
    eigminus = (1./np.sqrt(2))*np.matrix([[1j, 1]])
    return np.exp(-1j*theta/2)*np.matmul(eigplus.H,eigplus) + np.exp(1j*theta/2)*np.matmul(eigminus.H,eigminus)

PauliHerm = {'X':PX, 'Y':PY, 'Z':PZ}
PauliUnitaries = {'X':expX, 'Y':expY, 'Z':expZ}

def encode(x,n):
    ans = np.zeros((2**n,1))
    ans[x] = 1
    return ans
    
def decode(x,n):
    ans = [i for i in range(0,2**n) if x[i]==1]
    return ans[0]

def makesinglePauliGate(n, wire, gate, theta):
    ans = [1]
    gatefunc = PauliUnitaries[gate]
    for i in range(n):
        if i == wire:
            u = gatefunc(theta)
        else:
            u = Id
        ans = np.kron(ans,u)
    return ans

def makesinglePauliHerm(n, wire, gate):
    ans = [1]
    gateherm = PauliHerm[gate]
    for i in range(n):
        if i == wire:
            u = gateherm
        else:
            u = Id
        ans = np.kron(ans,u)
    return ans


def makeId(n):
    ans = [1]
    for _ in range(n):
        ans = np.kron(ans,Id)
    return ans

def measure0(n,w):
    ans = [1]
    m0 = [[1, 0], [0, 0]]
    for i in range(n):
        if i==w :
            ans = np.kron(ans,m0)
        else:
            ans = np.kron(ans,Id)
    return ans

def measure1(n,w):
    ans = [1]
    m1 = [[0, 0], [0, 1]]
    for i in range(n):
        if i==w :
            ans = np.kron(ans,m1)
        else:
            ans = np.kron(ans,Id)
    return ans

def composite_gate(n, Hs, theta):
    ans = makeId(n)
    for (i,(h,w)) in enumerate(Hs):
        ui = makesinglePauliGate(n,w,h,theta[i])
        ans = np.matmul(ui,ans)
    return ans

def grad_composite_gate(n, Hs, theta, idx):
    ans = makeId(n)
    for (i,(h,w)) in enumerate(Hs):
        ui = makesinglePauliGate(n,w,h,theta[i])
        hi = makesinglePauliHerm(n,w,h)
        ans = np.matmul(ui,ans)
        if i == idx : ans = np.matmul(-0.5j*hi,ans)
    return ans

def unentangled_unitary(wires):
    ans = []
    for i in range(wires) : ans.append(('X',i))
    for i in range(wires) : ans.append(('Y',i))
    for i in range(wires) : ans.append(('Z',i))
    return ans



def learn_with_control(wires, lfunc, 
                       epochs = 1000, step_size = 1e-3,verbose=False, seed=None):
    if seed != None: np.random.seed(seed)
    losses = []
    N = 2**wires

    #Parameterization of System
    Hs = unentangled_unitary(wires)
    theta = 1e-1*np.random.randn(len(Hs))
    Arho = 1e-1*np.random.randn(len(Hs))
    Brho = 1e-1*np.random.randn(len(Hs))

    #Training
    print('Learning with control, Labelling function: ', lfunc)
    for ep in range(epochs):
        if verbose: print('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
        
        # Build new gates
        U = composite_gate(wires,Hs,theta)
        U0 = composite_gate(wires,Hs,Arho)
        U1 = composite_gate(wires,Hs,Brho)

        # Initialize Gradients and Losses this epoch
        lloss = 0
        thgrad = np.zeros(len(Hs))
        Agrad = np.zeros(len(Hs))
        Bgrad = np.zeros(len(Hs))

        for z in range(2**wires):
            if lfunc[z]==0:
                my = measure0(wires,wires-1)
            else:
                my = measure1(wires,wires-1)
            m0 = measure0(wires,0)
            m1 = measure1(wires,0)
            inp = np.matmul(encode(z,wires),encode(z,wires).T)
            T = np.matmul(U,np.matmul(inp,U.H))
            mzero = np.matmul(m0,np.matmul(T,m0.H))
            mone = np.matmul(m1,np.matmul(T,m1.H))
            T0 = np.matmul(U0,np.matmul(mzero,U0.H))
            T1 = np.matmul(U1,np.matmul(mone,U1.H))
            prcorrect = np.real(np.trace(np.matmul(my,T0+T1)))
            lloss -= np.log(prcorrect)/N
            if verbose : print('Input: {}, Label: {}, Prob. of correct classification: {}'\
                                .format(z, lfunc[z], prcorrect))
            for i in range(len(Hs)):
                Ugrad = grad_composite_gate(wires,Hs,theta,i)
                Tgrad = np.matmul(Ugrad,np.matmul(inp,U.H)) + np.matmul(U,np.matmul(inp,Ugrad.H))
                mzero = np.matmul(m0,np.matmul(Tgrad,m0.H))
                mone = np.matmul(m1,np.matmul(Tgrad,m1.H))
                T0 = np.matmul(U0,np.matmul(mzero,U0.H))
                T1 = np.matmul(U1,np.matmul(mone,U1.H))
                thgrad[i] -= np.real(np.trace(np.matmul(my,T0+T1)))/(prcorrect*N)
            for i in range(len(Hs)):
                U0grad = grad_composite_gate(wires,Hs,Arho,i)
                T = np.matmul(U,np.matmul(inp,U))
                mzero = np.matmul(m0,np.matmul(T,m0.H))
                T0grad = np.matmul(U0grad,np.matmul(mzero,U0.H)) + np.matmul(U0,np.matmul(mzero,U0grad.H))
                Agrad[i] -= np.real(np.trace(np.matmul(my,T0grad)))/(prcorrect*N)
            for i in range(len(Hs)):
                U1grad = grad_composite_gate(wires,Hs,Brho,i)
                T = np.matmul(U,np.matmul(inp,U))
                mone = np.matmul(m1,np.matmul(T,m1.H))
                T1grad = np.matmul(U1grad,np.matmul(mone,U1.H)) + np.matmul(U1,np.matmul(mone,U1grad.H))
                Bgrad[i] -= np.real(np.trace(np.matmul(my,T1grad)))/(prcorrect*N)

        # Collect loss this epoch and update parameters
        theta = theta - step_size*thgrad
        Arho = Arho - step_size*Agrad
        Brho = Brho - step_size*Bgrad
        losses.append(lloss)
        print('Epoch : {}, negative log likelihood : {}'.format(ep,lloss))
        if verbose: print('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
    return losses
    


def learn_without_control(wires, lfunc, 
                        epochs = 1000, step_size = 1e-3, verbose=False, seed = None):
    if seed != None: np.random.seed(seed)
    losses = []
    N = 2**wires
    #Parameterization of System
    Hs = unentangled_unitary(wires)
    theta = 1e-1*np.random.randn(len(Hs))
    Arho = 1e-1*np.random.randn(len(Hs))

    #Training
    print('Learning without control, Labelling function: ', lfunc)
    for ep in range(epochs):
        if verbose: print('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
        
        # Build new gates
        U = composite_gate(wires,Hs,theta)
        U0 = composite_gate(wires,Hs,Arho)

        # Initialize Gradients and Losses this epoch
        lloss = 0
        thgrad = np.zeros(len(Hs))
        Agrad = np.zeros(len(Hs))
        for z in range(2**wires):
            if lfunc[z]==0:
                my = measure0(wires,wires-1)
            else:
                my = measure1(wires,wires-1)
            inp = np.matmul(encode(z,wires),encode(z,wires).T)
            T = np.matmul(U,np.matmul(inp,U.H))
            T0 = np.matmul(U0,np.matmul(T,U0.H))
            prcorrect = np.real(np.trace(np.matmul(my,T0)))
            lloss -= np.log(prcorrect)/N
            if verbose : print('Input: {}, Label: {}, Prob. of correct classification: {}'\
                                .format(encode(z,wires).T, lfunc[z], prcorrect))
            for i in range(len(Hs)):
                Ugrad = grad_composite_gate(wires,Hs,theta,i)
                Tgrad = np.matmul(Ugrad,np.matmul(inp,U.H)) + np.matmul(U,np.matmul(inp,Ugrad.H))
                T0 = np.matmul(U0,np.matmul(Tgrad,U0.H))
                thgrad[i] -= np.real(np.trace(np.matmul(my,T0)))/(prcorrect*N)
            for i in range(len(Hs)):
                U0grad = grad_composite_gate(wires,Hs,Arho,i)
                T = np.matmul(U,np.matmul(inp,U))
                T0grad = np.matmul(U0grad,np.matmul(T,U0.H)) + np.matmul(U0,np.matmul(T,U0grad.H))
                Agrad[i] -= np.real(np.trace(np.matmul(my,T0grad)))/(prcorrect*N)

        # Collect loss this epoch and update parameters
        theta = theta - step_size*thgrad
        Arho = Arho - step_size*Agrad
        losses.append(lloss)
        print('Epoch : {}, negative log likelihood : {}'.format(ep,lloss))
        if verbose: print('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
    return losses



if __name__ == "__main__":

    plot = True
    sd = 20
    wires = 4
    ep = 1000
    step = 1e-2
    
    lfunc = [1,0,1,0,1,0,1,0,0,1,0,1,0,1,0,1]
    
    l1 = learn_without_control(wires, lfunc, epochs=ep, verbose = False, step_size = step, seed = sd)
    print('Minimum Loss without Control {}'.format(np.amin(l1)))
    l2 = learn_with_control(wires, lfunc, epochs=ep, verbose = False, step_size = step, seed = sd)
    print('Minimum Loss with Control {}'.format(np.amin(l2)))

    if plot:
        np.save('withoutcontrol',l1)
        np.save('withcontrol',l2)

        fig = plt.figure()
        fig.suptitle('Learning function '+str(lfunc))
        fig, (axs1, axs2) = plt.subplots(1,2)
        axs1.plot(range(1,len(l1)+1),l1)
        axs1.set_title('No Control: {0:.6f}'.format(np.amin(l1)))
        axs2.plot( range(1,len(l2)+1),l2)
        axs2.set_title('Control: {0:.6f}'.format(np.amin(l2)))
        plt.savefig('Epochs{}_Learning Rate{}_Seed{}_Function_{}.png'.format(ep,step,sd,lfunc))

        fig = plt.figure()
        fig.suptitle('Learning function '+str(lfunc))
        fig, ax = plt.subplots()
        ax.plot(range(1,len(l1)+1),l1, label = 'Without control')
        ax.plot( range(1,len(l2)+1),l2, label = 'With control')
        ax.set_xlabel('Epochs')
        ax.set_ylabel('Negative log likelihood loss')
        ax.set_yticks(np.arange(0,2,0.1))
        ax.legend()
        plt.savefig('Epochs{}_Learning Rate{}_Seed{}_Function_{}_together_modified.png'.format(ep,step,sd,lfunc))