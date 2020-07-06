[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/fp-ctd-itmo/hw2-pavponn/blob/master/LICENSE)

# 💻 Watcher: File Manager & Version Control System
Watcher is a file manager with built-in support of version control system implemented in Haskell.

## Summary 

- Initial directory treated as a root by this file manager. It means that it's only possible to work with initial directory and its subdirectories.
- You can use both relative and absolute paths. However, "absolute" path starts at your initial folder. Here is an example:
initial directory: `/Users/user/a/`, then absolute path is `/b/c/d`, not `/Users/user/a/b/c/d`.
- It's possible to init VCS in every folder of the system, however you can initialize VCS in each folder only once (same as it's done in git). `Current VCS` is the closest VCS to your current directory.
- Unfortunately, VCS is not capable to commit file deletion :(

P.S. I know, that code is a bit messy, but hope it's not so bad :/

## Tests
Tested manually on macOS Catalina 10.15.4.

Autotests were written using Hspec.

## Copyright
Pavel Ponomarev, 2020

MIT License.
