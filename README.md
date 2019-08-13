
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

Numbers on

## Language Reference

### Arithmetical Operators

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


### Notational Functions
| Input | Representation | Description |
|:------|:---------------|:------------|
| `sqrt[1/2]` | <img width="150" src="https://user-images.githubusercontent.com/2157285/62938357-27d56380-bde4-11e9-8d10-e58004d5e2f5.png"> | Square Root |
| `abs[1/2]` | <img width="138" src="https://user-images.githubusercontent.com/2157285/62938360-29069080-bde4-11e9-9972-a99bdfbdc0cb.png"> | Absolute |
| `ceil[1/2]` | <img width="140" src="https://user-images.githubusercontent.com/2157285/62938361-29069080-bde4-11e9-8bdd-70096f14185c.png"> | Ceiling |
| `floor[1/2]` | <img width="138" src="https://user-images.githubusercontent.com/2157285/62938362-29069080-bde4-11e9-8069-8d681c0ada8d.png"> | Floor |

### Functions

| Function | Description      |
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
| Alpha' | Α |
| Alpha | α |
| Beta' | Β |
| Beta | β |
| Chi' | Χ |
| Chi | χ |
| Delta' | Δ |
| Delta | δ |
| E | e |
| Epsilon' | Ε |
| Epsilon | ε |
| Eta' | Η |
| Eta | η |
| Gamma' | Γ |
| Gamma | γ |
| Iota' | Ι |
| Iota | ι |
| Kappa' | Κ |
| Kappa | κ |
| Lambda' | Λ |
| Lambda | λ |
| Mu' | Μ |
| Mu | μ |
| Nu' | Ν |
| Nu | ν |
| Omega' | Ω |
| Omega | ω |
| Omicron' | Ο |
| Omicron | ο |
| Phi' | Φ |
| Phi | φ |
| Pi' | Π |
| Pi | π |
| Psi' | Ψ |
| Psi | ψ |
| Rho' | Ρ |
| Rho | ρ |
| Sigma' | Σ |
| Sigma | σ |
| Tau' | Τ |
| Tau | τ |
| Theta' | Θ |
| Theta | θ |
| Upsilon' | Υ |
| Upsilon | υ |
| Xi' | Ξ |
| Xi | ξ |
| Zeta' | Ζ |
| Zeta | ζ |