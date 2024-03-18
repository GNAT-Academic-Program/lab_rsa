# RSA Lab

## Prerequisite

- Alire [Setup Instructions](https://github.com/GNAT-Academic-Program#install-alire-an-ada-package-manager)
- Alire GAP index [Setup Instructions](https://github.com/GNAT-Academic-Program#add-the-gap-alire-index-important)
- Tmux

### Install Tmux
```console
sudo apt-get install tmux
```  

### Fetch the Crate
```console
alr get rsa_lab && cd rsa_lab*
```

### Build (Alire)
```console
alr build
```
### Build (GPRBuild)
```console
eval "$(alr printenv)" && gprbuild rsa_lab.gpr
```

### Build (GnatStudio)
```console
eval "$(alr printenv)" && gnatstudio rsa_lab.gpr
```

### Run

```console
cd bin && ./setup_tmux.sh
```   

## Contributing

Your contributions are welcome! If you wish to improve or extend the capabilities of this BSP, please feel free to fork the repository, make your changes, and submit a pull request.

## Support and Community

If you encounter any issues or have questions regarding the usage of this crate, please file an issue on the GitHub repository. 
For broader discussions or to seek assistance, join an Ada developer community found [here](https://github.com/ohenley/awesome-ada?tab=readme-ov-file#community).

---

Happy Coding with Ada!



