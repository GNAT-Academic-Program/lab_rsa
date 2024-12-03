# Lab 3 Instructions

- Note that the theory explanations were powered by my research in combination with ChatGPT.
## How do we decrypt a ciphertext ?
To decrypt ciphertext, we make use of the Decrypt_Msg function. From this function (and the nested helper functions it calls), we are able to achieve the necessary steps to build a decrypted message from the given ciphertext and the required keys.

The steps are as follows: 
- Determine the beginning and end of the received ciphertext.
- Count the number of chunks in the ciphertext.
- Decrypt each chunk into its original integer value and convert this into string format.
- Append each decrypted string format of the chunk to a string variable which will resulted into the constructed plaintext.

### Step 1: Determining the Start and End of Ciphertext:
- This step is done at the very beginning of the Decrypt_Message function.
- We can find the start and end of the given ciphertext (which is simply a string variable).
- The start is found by using the 'First attribute of the ciphertext.
- The end is found with using the 'Last attribute of the ciphertext.
- Both values are stored in variables called S and E.
```console
Your job: fill in the missing code to correctly determine the value of S and E.
```

### Step 2: Count the number of chunks in the ciphertext:
- In the helper function Number_Of_Words, we count the number of chunks to decrypt.
- Looping through the range of the ciphertext, we check each character of the ciphertext for a comma. If it is, this means that we found a chunk.
- This means we increment our counter that tracks the number of chunks in the ciphertext.
```console
Your job: fill in the missing code to correctly implement the logic for finding the number of words.
```

### Step 3: Creating an array of encrypted words:
- Once the loop is complete, we instantiate an array that will hold the decrypted chunk which is initialized with 0 for each space. Its size is the same as the counter of the number of chunk. This is done outside the Number_Of_Words function and is named W.
- This step makes use of the main functionality in the Decrypt_Msg function. 
- To do this, we loop through the range of W and fill the array at each iteration though getting the integer value of every chunk. Each chunk is obtained through the helper function Find_Next_Word. This function takes the cyphertext, Start and End positions as parameter.
- At each iteration, after a chunk's integer value is obtained, the Start index is increased by 2 to find the position of the next chunk to be processed.
```console
Your job: fill in the missing code to correctly implement the logic for finding integer value of every word and filling the W array accordingly. Note that you wil have to transform it to a Big_Integer by using the From_String helper function.
```

### Step 4: Decrypt each word and construct the plaintext:
- Similar to building the encrypted message, we recursively build the plaintext with the Build_Decrypted_Msg function. 
- If the current index is less than the last position of the given word array, the word at that position in the array is decrypted using the previously defined Decrypt function from lab 1. 
- The next word is decrypted by calling the same function once more with the same array, just with the index position increased by one.
- If not, this indicates that we have reached the last word in the cipher text and it is decrypted.
- Once completed, each decrypted chunk has successfully been appended to the plaintext string.
```console
Your job: fill in the missing code to correctly implement the logic for the recursive function that correctly builds the plaintext. Note that eahc decrypted chunk will have to be converted to string with the function To_Str.
```