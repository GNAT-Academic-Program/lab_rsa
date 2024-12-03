# Sockets Lab

## Socket Communication
- Sockets are used to estalish bi-directional communication between two clients on a network.
- FIFO is the method used to send and receive data between the two clients.
- Each socket is an endpoint of the communication link betwen the clients. 
- Sockets are identified by the IP address and port number it is connected to. 
- This communication protocol can be established using Ada's GNAT Sockets library.

## Basics of GNAT Sockets
- To create a socket, you can ues the command:
```ada
Create_Socket(Socket => GNAT.Sockets.Socket_Type);
```
- You can declare a socket type as follows:
```ada
Socket_Type : GNAT.Sockets.Socket_Type;
```
- If you want your socket to reuse an address, you can specify this in the options of the function that sets a socket:
```ada
Set_Socket_Option(Socket => Socket_Type,
Level => GNAT.Sockets.Socket_Level,
Option => (Reuse_Address => True));
```
- To bind a socket to a specific inet localhost address and port number, specify the INET address of localhost and the port number in the GNAT.Sockets.Bind_Socket function:
```ada
Bind_Socket
(Socket => Socket_Type,
Address => (Family => GNAT.Sockets.Family_Inet,
Addr => GNAT.Sockets.Inet_Adr("localhost number"),
Others => (Port => Port_Number)));
```
- To create a server socket that listens for incoming connections, the following function can be used:
```ada
GNAT.Sockets.Listen_Socket (Socket => Receiver);
```
- For the server socket to accept incoming connections, the following function can be used:
```ada
GNAT.Sockets.Accept_Socket
        (Server => Receiver,    --  creates server socket that listens for connection attempts
         Socket => Connection,  -- creates client socket of type Connection
         Address => Client);    --  address of the client that connected
```
- To enable input and output streams to the socket, use the following functions:
```ada
GNAT.Sockets.Stream (Socket => Socket_Type);
```
- To close a socket, use the Close_Socket function:
```ada
GNAT.Sockets.Close_Socket (Socket => Socket_Type);
```
## Extra tips of GNAT Sockets:
- While listening for inoming connections, the methods with GNAT sockets should be in a loop, and intialized by the "Find" keyword.
- For thread-safe communication, a FIFO queue shuold be implemented with a stack (and its respective pop and push functions).
- The stack should be popped when a new client connection is established, and pushed when a client disconnects.
- To send string over a socket, we use the function String'Write (Stream, what you want to send in string format).
- To read from a socket, we use the function Character'Input (Stream).
- If you would like to add a receivced messages into an array, you can do the following:
```ada
arrayMessages (index).Add (new message);
```

## Your Task:
- Complete the missing parts of the server_main.adb file.

### Hints:
- The array of messages is defined as an Ada type called:
```ada
type Messages_List is array (1 .. Tasks_To_Create) of Messages;
```
where Tasks_To_Create is a constant integer that defines the number of available clients we can have, namely, two.
- Messages is another type that is essentially an unbounded string with two procedures: Add and Get.
- There is a predefined task that setup the communication between server and client sockets called Setup. There is another predfined task that handles the read and write oeprations of the sockets called Read_Write. These are called by the array of SocketTasks. 
- SocketTasks are able to read and write (implemented in respective tasks).
- The server socket is implemented as SocketServer.
- The whole functionality is intialized by the Echo_Server variable (of type SocketServer) and is executed by calling its Listen function.
- The stack functionality is found in the Info type implementation. 
- When adding messages to the message array, you need to ensure that the message is sending (aka appending) to the receivers index. In essence, if the sending index is 1, it shoud go to index 2 (so client 2 can receive it), and vice versa.
- Once a client is finish writing, it shoud clear the variable thatholds its message with the Delete function.
```ada
Delete (Message string value, string's starting position , number of characters to delete);
```