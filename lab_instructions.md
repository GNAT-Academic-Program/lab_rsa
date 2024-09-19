# Lab Instructions

## Install Alire 
### What is Alire ? 
Alire is the package manager for the Ada programming language.    
Alire is to Ada like what pip is to Python.

### Why you need Alire:
Alire is crucial to building and compiling this project.

### MacOS
#### Download Alire for MacOS:    
- Navigate to https://github.com/alire-project/alire/releases/latest
- Download the file named alr-[latest_version]-bin-x86_64-macos.zip, where [latest_version] will be the most current version number.
- Unzip the downloaded file. 
- Rename the unzipped folder to just `alire`.

#### Add the `alr` binary to your path:
- Open the terminal and run:
```console
echo "$HOME/Downloads/alire/bin" | sudo tee /etc/paths.d/alr_config > /dev/null
```
- Change the permission of the created file with the following command:
```console
sudo chmod 644 /etc/paths.d/alr_config
```
- Update the machine's PATH:    
```console
/usr/libexec/path_helper -s
```
- Check if alr is recognized: Run the command below. It should return a valid path. If not, restart the installation.
```command
which alr
```
