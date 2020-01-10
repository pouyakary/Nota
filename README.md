
# Nota

Nota is a powerful terminal calculator with rich mathematical notation and chart rendering. It gives you a beautiful language that is readable and easy to type and then renders your input into a beautiful notation with result:

<img width="762" src="https://user-images.githubusercontent.com/2157285/62937043-99abae00-bde0-11e9-8ce5-5d7257fa7b28.png">

<img width="762" src="https://user-images.githubusercontent.com/2157285/62937044-99abae00-bde0-11e9-8f6c-c756ba789da8.png">

## Installation

First you should have [GNU Make]() and [Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) installed. Then you have to run these commands within your terminal to install nota. This will give you the command `nota` that you can run

```bash
git clone http://codes.kary.us/nota/nota.git
cd nota
make install
```

## Language Grammar

| Grammar Component | Description |
|:------------------|:------------|
| Normal Numbers | Numbers in Nota can be in the normal form. Integer form like `0` or `42480`. And in the decimal form like `1.8402` or `0.5` but remember decimal only numbers cannot be without zero: so `0.5` is possible but `.5` is not. |
| Hex Number | Hexadecimal Numbers are supported and must be started with the `0x` sign. So `0xfff` is a number as well as `0x123`. |
| Names | Nota approaches identifiers much differently than any other language. In Nota identifiers can have space within them, so no more `camelCase`, `PascalCase`, `--kebab-case`, `what_ever`; You can simply write things like `size of the planet` and it works.<br><br>Also to make it more interesting, Nota even gives you `'` (apostrophe) and therefore you can have things like `radius of earth` or `earth's radius`.<br><br> Numbers are also allowed (but not for the first letter). You can have names like: `X5` or `X 5` and etc. |
| Binary Operator | Binary operators in Nota are: `+`, `-`, `*`, `/`, `^`, `%`, `?`, and `!`. They are fully explained in the _Binary Operators_ section of the _Language Reference_. |
| Negation | The only unary operator that Nota defines is the value negation `-` operator. E.g: `-13 + -sin[x]`.
| Parenthesis | Nota provides the parenthesis notation to reorder precedence like: `(1 + 2) * 3`.
| Function Calls | Functions in Nota are written not with parenthesis but with brackets (`Sin[x]`, `Log[2, 100]`, ...).<br><br> Just like identifiers they are case insensitive so it doesn't matter how you write them: `log[x]` = `LOG[x]` = `lOG[x]`<br><br>For the sake of beauty, some of the functions are rendered specially (explained in the _Notational Functions_ section). Also in the reference you can find a full explanation of the functions.|
| Name Assignment | You can assign names to calculations for further use. These names are constants and you have to use the assignment grammar to register them. `Name = Value`. So something like: `Earth's Volume = 3/4 * pi * Earth's Radius^3`.

## Control Commands
The control commands are not parts of the language, but rather the controllers of the application itself:

| Command | Description |
|:--------|:------------|
| `help`  | Shows you the link to the documentations |
| `exit`  | Exists from the application |
| `clear` | Clears the calculator screen |

## Language Reference

### Binary Operators

| Input      | Representation      | Description    |
|:-----------|:--------------------|:---------------|
| `1 + 2` | <img width="124" src="https://user-images.githubusercontent.com/2157285/62937961-45ee9400-bde3-11e9-938b-b9a77b049a54.png"> | Summation |
| `1 - 2` | <img width="122" src="https://user-images.githubusercontent.com/2157285/62938130-a54ca400-bde3-11e9-9cbe-8f0f63590c16.png"> | Subtraction |
| `1 * 2` | <img width="125" src="https://user-images.githubusercontent.com/2157285/62938132-a54ca400-bde3-11e9-8cf3-8437c388dd8a.png"> | Multiplication |
| `1 / 2` | <img width="108" src="https://user-images.githubusercontent.com/2157285/62937962-45ee9400-bde3-11e9-8262-e3c32e9b7617.png"> | Division |
| `1^2` | <img width="102" src="https://user-images.githubusercontent.com/2157285/62937963-45ee9400-bde3-11e9-88ee-7b640ef14cc0.png"> | Power |
| `1 % 2` | <img width="124" src="https://user-images.githubusercontent.com/2157285/62941253-dbd9ed00-bdea-11e9-8f69-b51afacf56a2.png"> | Modulo |
| `1 ? 1` | <img width="120" src="https://user-images.githubusercontent.com/2157285/62939696-1b9ed580-bde7-11e9-8566-d60143b04e3a.png"> | Equals <br> _Returns 1 if equals and 0 otherwise_ |
| `1 ! 1` | <img width="117" src="https://user-images.githubusercontent.com/2157285/62939697-1b9ed580-bde7-11e9-8dd1-42783e23a6b2.png"> | Not Equals <br> _Returns if not equals and 0 otherwise_ |

Keep in mind that the precedence of the operators are as:

1. `^`
0. `*`, `/`, `%`
0. `+`,  `-`
0. `?`, `!`


### Notational Functions
| Input | Representation | Description |
|:------|:---------------|:------------|
| `sqrt[1/2]` | <img width="150" src="https://user-images.githubusercontent.com/2157285/62938357-27d56380-bde4-11e9-8d10-e58004d5e2f5.png"> | Square Root |
| `abs[1/2]` | <img width="138" src="https://user-images.githubusercontent.com/2157285/62938360-29069080-bde4-11e9-9972-a99bdfbdc0cb.png"> | Absolute |
| `ceil[1/2]` | <img width="140" src="https://user-images.githubusercontent.com/2157285/62938361-29069080-bde4-11e9-8bdd-70096f14185c.png"> | Ceiling |
| `floor[1/2]` | <img width="138" src="https://user-images.githubusercontent.com/2157285/62938362-29069080-bde4-11e9-8069-8d681c0ada8d.png"> | Floor |

### Functions

| Control Function | Description |
|:-----------------|:------------|
| `Out[x]` | Returns the computed value of the output no. `x` within the computed history. In case of output being a table, it returns the very first computed result<br><img width="252" src="https://user-images.githubusercontent.com/57100914/72173461-d9f17580-33ec-11ea-9bda-093c4dc9cca4.png"> |

<br>

| Arithmetical Function | Description      |
|:-------------------|:-----------------|
| `Log[x]` | Logarithm of `x` of base `e` |
| `Log[b, x]` | Logarithm of `x` of base `b` |
| `Sin[x]` | Sine of `x` |
| `Cos[x]` | Cosine of `x` |
| `Tan[x]` | Tangent of `x` |
| `Cot[x]` | Cotangent of `x` |
| `Sec[x]` | Secant of `x` |
| `Csc[x]` | Cosecant of `x` |
| `Asin[x]` | Area Sine of `x` |
| `Acos[x]` | Area Cosine of `x` |
| `Atan[x]` | Area Tangent of `x` |
| `Sinh[x]` | Hyperbolic Sine of `x` |
| `Cosh[x]` | Hyperbolic Cosine of `x` |
| `Tanh[x]` | Hyperbolic Tangent of `x` |
| `Coth[x]` | Hyperbolic Cotangent of `x` |
| `Sech[x]` | Hyperbolic Secant of `x` |
| `Csch[x]` | Hyperbolic Cosecant of `x` |
| `Asinh[x]` | Hyperbolic Area Sine of `x` |
| `Acosh[x]` | Hyperbolic Area Cosine of `x` |
| `Atanh[x]` | Hyperbolic Area Tangent of `x` |
| `Max[a, ..., b]` | Maximum of the argument |
| `Min[a, ..., b]` | Minimum of the argument |
| `Sum[a, ..., b]` | Arguments sum |
| `Exp[x]` | Natural exponent to the power of `x` |


### Identifiers with Reserved Notation
For a more beautiful rendering, Nota reserves some identifiers to be used for rendering famous characters out of a normal keyboard. Expect for the `Pi`, every other identifier is available to be declared:


<img width="256" src="https://user-images.githubusercontent.com/2157285/62942324-4855eb80-bded-11e9-8f35-81d234521ca2.png">


| Name | Notation |
| :-------- | :---------- |
| `Alpha'` | Α |
| `Alpha` | α |
| `Beta'` | Β |
| `Beta` | β |
| `Chi'` | Χ |
| `Chi` | χ |
| `Delta'` | Δ |
| `Delta` | δ |
| `E` | e |
| `Epsilon'` | Ε |
| `Epsilon` | ε |
| `Eta'` | Η |
| `Eta` | η |
| `Gamma'` | Γ |
| `Gamma` | γ |
| `Iota'` | Ι |
| `Iota` | ι |
| `Kappa'` | Κ |
| `Kappa` | κ |
| `Lambda'` | Λ |
| `Lambda` | λ |
| `Mu'` | Μ |
| `Mu` | μ |
| `Nu'` | Ν |
| `Nu` | ν |
| `Omega'` | Ω |
| `Omega` | ω |
| `Omicron'` | Ο |
| `Omicron` | ο |
| `Phi'` | Φ |
| `Phi` | φ |
| `Pi'` | Π |
| `Pi` | π |
| `Psi'` | Ψ |
| `Psi` | ψ |
| `Rho'` | Ρ |
| `Rho` | ρ |
| `Sigma'` | Σ |
| `Sigma` | σ |
| `Tau'` | Τ |
| `Tau` | τ |
| `Theta'` | Θ |
| `Theta` | θ |
| `Upsilon'` | Υ |
| `Upsilon` | υ |
| `Xi'` | Ξ |
| `Xi` | ξ |
| `Zeta'` | Ζ |
| `Zeta` | ζ |