# Enigma-Elm (Elmigma)
This project implements the Enigma machine in Elm.  This project is part of the course Functional Frontend Development at the 
Flensburg University of Applied Sciences.

1. The compiled project is available under the following link: http://shauck.ddns.net:8080/ss19_enigma_server/.
2. The project is also available under the following link: https://simonhauck.github.io/Enigma-Elm/. The server functionality might not be available with this link! 

## Building from source

1. Checkout Project
2. Build index file: `elm make src/Main.elm`
3. Open Index File

## Options

This Enigma offers the following options:
1. Select different rotors
2. Specify the rotor and ring position
3. Specify the reflector
4. Specify the plugboard
5. Generate a Random key
6. Offer an Option to include or ignore foreign chars. 
7. The complete substitution process can be displayed in an Svg element
8. Select different encryption/decryption Speeds
    * Single Step Encryption/Decryption
    * Instant Encryption/Decryption
    * Automatic interval that can be modified with a slider
9. The result can be sent to a server. Other users can see your message and decrypt it with the correct key

## How-to Encrypt/Decrypt messages

1. First you need an Input. Set the input text into the textarea with the headline 'Text Input'
    * For Encryption this is your message. 
    * For Decryption this is the encrypted message
2. Set the key. 
    * For Encryption: Generate a random key with the 'Generate Random Key' button or set the values manually
    * For Decryption: Set the Key that was used for the Encryption. If you don't know the key, you can't decrypt the 
    message
3. Switch from the Configuration mode to the Encryption mode by clicking on the Button 'Switch to Encryption Mode'
4. Encrypt/Decrypt the message
    * You can enable the automatic encryption. You can set the speed with the slider above. The automatic encryption
    will encrypt every x milliseconds a character.
    * You can click the 'Single Step' button. Every time you press this button a character will be encrypted/decrypted
    * You can click the 'Instant Encryption' button. This will encrypt/decrypt the complete input text.

## Further Information

1. The Functionality of the Enigma is described here:  https://github.com/stephl001/EnigmaMachine
2. The Rotor and Reflector Specification can be found here: https://github.com/stephl001/EnigmaMachine/wiki/Rotors-Specifications

## Resources

Background Image: https://wallhere.com/en/wallpaper/980234

