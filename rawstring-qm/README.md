
Simple library to make haskell more user friendly.

Providing multiline strings and maybe string interpolation form (String -> String) function

Usage

No interpolation:
 [qq|
sadsdasd
asdsad
dsaad
|]

let asb="bla"
[qm| bla $asb bla |]
[qm| bla bla bla |]
[qm| bla bla {adv} |] (`lookup` [("adv","bla")])
[qm| bla $bla {bla} |] (`lookup` [("adv","bla")])

all would give "bla bla bla"



