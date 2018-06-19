# "GF(仮) カードギャラリー" Downloader

### Description
"GF(仮) カードギャラリー" is Unofficial Site of the "ガールフレンド（仮） by Ameba".  
This program is download support program to that site published pictures.  
This program is written by "Common Lisp", and assume sbcl.

### Dependency Library
- dexador
- plump
- clss  
this project used "quickload". Therefore need setup the "quicklisp".  
Let's do below commands.  
```
curl -O https://beta.quicklisp.org/quicklisp.lisp
curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
sbcl --load quicklisp.lisp
```

Did above commands? If done, run below commands at "SBCL REPL".  
```
(quicklisp-quickstart:install)
(ql:add-to-init-file)
```

If you wish uninstall quicklisp.  
```
rm -rf ~/.quicklisp
```

#### Example run
```
sbcl --load main.lisp
```

at SBCL REPL  
```
59
```
Then input number. Please reference from each girls detail page url of the "GF(仮) カードギャラリー".  
#### Example Number
```
http://gfkari.gamedbs.jp/girl/detail/1 <- Let's input this number
```
