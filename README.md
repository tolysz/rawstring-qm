# rawstring-qm 
[![Coverage Status](https://img.shields.io/coveralls/tolysz/rawstring-qm.svg)](https://coveralls.io/r/tolysz/rawstring-qm)
[![Build Status](https://travis-ci.org/tolysz/rawstring-qm.svg?branch=master)](https://travis-ci.org/tolysz/rawstring-qm)
[![Latest Version](https://img.shields.io/hackage/v/rawstring-qm.svg)](https://hackage.haskell.org/package/rawstring-qm)



Simple library to make haskell more user friendly.

Providing multiline strings and maybe string interpolation form (String -> String) function

Usage

No interpolation:

     [qq|
    sadsdasd
    asdsad
    dsaad
    |] == :: String

    let asb="bla"
        adv="bla"
    [qm| bla $asb bla |]
    [qm| bla bla bla |]
    [qm| bla bla ${adv} |]
    [qm| bla $asb ${adv} |]

    [qt| |] -- like qm but produces :: Text and uses ToText before concat
    [qtl| |] --- Text.Lazy 

all would give "bla bla bla"



