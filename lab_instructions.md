# Lab 2 Instructions

- Note that the theory explanations were powered by my research in combination with ChatGPT.
## How are messages encrypted ?
To encrypt plaintext, we make use of the Encrypt_Msg function. From this function (and the helper functions it calls), we are able to achieve the necessary steps to build an encrypted message from the given plaintext and the required keys.

The steps are as follows: (as summarized by ChatGPT)
- Sanitize the given plaintext.
- Split the sanitized message into chunks.
- Convert each chunk into an integer.
- Encrypt each integer.
- Construct a string from the encrypted integers and returns the string (ie. the ciphertext).

### Step 1: Sanitization of Plaintext:
- In this step, we make use of the Sanitize_Msg function that takes the plaintext as string as a parameter. First, the function calculates the number of padding bytes needed to make the plaintext a multiple of 2. This program has set the number of bytes per chunk to be 2. This number is held in a variable called To_Pad.
- The sanitized message is created by concatenating the plaintext with asterisks. Concatention is done with the & symbol. The number of asterisks added is the number of padding bytes calculated for To_Pad.
- The sanitized message is returned.
```console
Your job: fill in the missing code in order to complete the required calculations for the this step.
Please also fill in the code to create and return the sanitized message.

HINT: this can be calculated with the use of the modulo function in conjuction with a simple subtraction. The two variables in the equation are the number of bytes per chunk and the plaintext length. There are two operations (one subtraction and a modulo division) that take place during this calculation.
```

### Step 2: Splitting of the Sanitized Plaintext into Chunks:
- The number of chunks in the plaintext is found using the following calculation:
plaintext length / number of bytes per chunk
- A W array is instatiated and initially holds an integer value of 0. This and the previous step are done in the nested function called Number_Of_Words.
- The sanitized plaintext is split into chunks by determining the number of indexes it has, then using its respecitive index to get the associated chunk of the sanitized message.
- Each chunk is a character in the overall plaintext. For example, if the sanitized message is "adacore", the chunks are "a", "d", "a", "c", "o", "r", "e". A chunk can also be a filler character, namely "*".
- For the length of W, we loop through the range of W and grab the respetive chunk of the sanitized message using the index position. 
- Once the chunk is obtained, we convert it to an Integer withthe To_Int helper function and store it at the proper index of W.
- The index Idx is instantiated as an integer using the equation:
```console
 ((position in W's range (namely I) -1) * number of bytes per chunk) + 1.

```console
Your job: fill in the missing code to implement the logic for instantiating the W array of type Words (an array of strings). The type Words has already been instantiated.
```

### Step 3: Encrypting each integer:
- This step is done in the main functionality of Encrypt_Msg function. We loop through the entire range of the W array.
- At each iteration, we call the Encrypt function with its necessary parameters. Each iteration represents the encryption of each 'integerized' chunk having encryption done upon it and stored in the W array.
```console
Your job: fill in the missing code to implement the logic for the encryption process. 
```

### Step 4: Constructing the ciphertext:
- Here we make use of the Build_Encrypted_Msg by passing it the word array, which now has the encrypted representation of the message in integers instead of just plaintext integers. 
- The goal of the Build_Encrypted_Msg function is to construct a single string that represents the string of integers that represent the encrypted message. Each integer that represents a chunk of the message is trimmed. The final string contains all integers that represent a chunk of the ciphertext and all are comma separated.
- We recursively build the ciphertext where we determine if the index is less than the last index of the encrypted word it received. 
- If this is true, this means we are not finished building the string. In this case, we trim the chunk at the respective index, append a comma and recursively call the function wiht the index increased by one.
- If not true, this means that we have reached the last chunk of the ciphertext. We would simply trim the chunk and append a comma without calling the function again.

```console
Your job: fill in the missing code to implement the logic for the collection and concatenation of the encrypted words. 
```

## The next lab will cover how RSA decryption works and how to build the plaintext from the ciphertext.
