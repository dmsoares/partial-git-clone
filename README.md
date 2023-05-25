# (Very much) A Partial Git Clone

This project is a rewrite of the [Write Yourself A Git (WYAG)](https://wyag.thb.lt/) git clone in Haskell (the original being written in Python). The main differences arise naturally from the differences in language and programming paradigm. 

The initial motivation for this endeavour was one of penance. After a couple of embarrassing mistakes using Git professionally, something had to be done.

The choice of using Haskell has to do with the fact that merely following a tutorial would not be enough. The process of translating from one language to the other really has to make me understand what is going on. As a couple of bonus points, my skills with Haskell grow and my eyes get a bit more comfortable with Python.

Update: In the meantime, I've found out about [this similar project in Haskell](https://vaibhavsagar.com/blog/2017/08/13/i-haskell-a-git/).
While its scope is smaller, its author's motivations are very close to my own. I've been learning a lot from it as well.
Git's gritty details are in the [Git Book](https://git-scm.com/book/en/v2)

## Commands
Following [WYAG](https://wyag.thb.lt/), this Git clone should implement the following commands:
- [ ] add 
- [x] cat-file 
- [ ] checkout
- [ ] commit 
- [x] hash-object 
- [x] init 
- [x] log 
- [ ] ls-files 
- [x] ls-tree 
- [ ] merge 
- [ ] rebase 
- [ ] rev-parse 
- [ ] rm 
- [ ] show-ref 
- [ ] tag 