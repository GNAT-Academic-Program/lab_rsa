# Lab 2 Instructions

## How are messages encrypted ?
To encrypt plaintext, we make use of the Encrypt_Msg function. From this function (and the helper functions it calls), we are able to achieve the necessary steps to build an encrypted message from the given plaintext and the required keys.

The steps are as follows: (as summarized by ChatGPT)
- Sanitize the given plaintext.
- Split the sanitized message into chunks.
- Convert each chunk into an integer.
- Encrypt each integer.
- Construct a string from the encrypted integers and returns the string aka the cyphertext.

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
- The W array is filled with the sanitized chunks in the main part of the Encrypt_Msg function. This is indicated by the first instance of the keyword 'begin' in the function. Idx is instantiated as an integer that is the current index using the equation position in W's range -1 * number of bytes per chunk + 1.
- The rest is done through looping through the range of W and storing each chunk of the sanitized message in the W array. 
```console
Your job: fill in the missing code to implement the logic for instantiating the W array of type Word (an array of strings). The type Word has already been instantiated.
```

### Step 3: Converting each chunk into an integer.
- Each chunk is converted into an integer using the Build_Encrypted_Msg Function. 
The function determines if the index is less than the last index of the array that will hold the integer represntaiton of each chunk of the message. 
- If yes, it adds a comma to the chunk and trims the chunk of leading and trailing whitespaces of the converted chunk. The chunk is converted using the 'Image function. Then, it calls the method again, passing the W array and the next index position.
- If no, this means that we have reached the last chunk in the W array. We return a comma and the trimmed version of the converted chunk.
```console
Your job: fill in the missing code to implement the logic for the conversion.
```

### Step 4: Encrypting each integer:
- This step is done in the main functionality of Encrypt_Msg function. We loop through the entire range of the W array.
- At each iteration, we call the Encrypt function with its necessary parameters. Each iteration represents the encryption of each 'integerized' chunk having encryption done upon it and stored in the W array.
```console
Your job: fill in the missing code to implement the logic for the encryption process. 
```

### Step 5: Constructing the cyphertext:
- Once again, we make use of the Build_Encrypted_Msg by passing it the word array, which now has encrypted integers instead of just plaintext integers. 
- This time, we make sure to concatenate the each return into a single String constant called Encrypted_Msg. The call to Build_Encrypted_Msg and concatenation occurs in the main functionality of Encrypt_Msg.
- Once finished, Encrypted_Msg is returned.
```console
Your job: fill in the missing code to implement the logic for the collection and concatenation of the encrypted words. 
HINT: the final encrypted word will be stored in an array called Encrypted_Msg.
```

## How do we decrypt a ciphertext ?
To decrypt ciphertext, we make use of the Decrypt_Msg function. From this function (and the nested helper functions it calls), we are able to achieve the necessary steps to build a decrypted message from the given ciphertext and the required keys.

The steps are as follows: (as summarized by ChatGPT)
- Determine the beginning and end of the given ciphertext.
- Count the number of words in the ciphertext.
- Gather all encrypted words into a single Word array called W.
- Decrypt each word and construct the plaintext.

### Step 1: Determining the Start and End of Ciphertext:
- This step is done at the very beginning of the Decrypt_Message function.
- We can find the start and end of the given ciphertext (which is simply a string variable.)
- The start is found by adding 1 to the ciphertext's first index.
- The end is found with the ciphertext's last index.
- Both values are stored in variables called S and E.
```console
Your job: fill in the missing code to correctly determine the value of S and E.
```

### Step 2: Count the number of words in the ciphertext:
- Looping through the ciphertext, we look for any instance of a comma in the Number_Of_Words function.
- If we find such an instance, we increment our counter that tracks the number of words in the ciphertext.
- Once the loop is complete, we instantiate an array that will hold the decrypted words which is initialized with 0 for each space. Its size is the same as the counter of the number of words. This is done outside the Number_Of_Words function and is named W.
```console
Your job: fill in the missing code to correctly implement the logic for finding the number of words.
```

### Step 3: Creating an array of encrypted words:
- This step makes use of the main functionality in the Decrypt_Msg function. To do this, we loop through the range of W and fill the array at each iteration though getting the integer value of every word. Each word is obtained through the nested function Find_Next_Word. This function takes the cyphertext, Start and End positions as parameter.
- At each iteration, after a word's integer value is obtained, the Start index is increased by 2 to find the position of the next word to be processed.
```console
Your job: fill in the missing code to correctly implement the logic for finding integer value of every word and filling the W array accordingly.
```

### Step 4: Decrypt each word and construct the plaintext:
- Making use of the Build_Decrypted_Msg function, we build the plaintext by using a recursive function on the W array.
- If the current index is less than the last position of the given word array, the word at that position in the array is decrypted using the previously defined Decrypt function from lab 1. The next word is decrypted by calling the same function once more with the same array, just with the index position increased by one.
- If not, this indicates that we have reached the last word in the cipher text and it is decrypted.
- The construction of the plaintext occurs after the last line of code for step 3, in the main functionality of Decrypt_Msg. 
```console
Your job: fill in the missing code to correctly implement the logic for the recursive function that correctly builds the plaintext.
```

