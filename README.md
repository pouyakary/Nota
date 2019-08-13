
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
| `1 + 2` | <img width="124" src="https://user-images.githubusercontent.com/2157285/62937961-45ee9400-bde3-11e9-938b-b9a77b049a54.png"> | **Summation** |
| `1 - 2` | <img width="122" src="https://user-images.githubusercontent.com/2157285/62938130-a54ca400-bde3-11e9-9cbe-8f0f63590c16.png"> | **Subtraction** |
| `1 * 2` | <img width="125" src="https://user-images.githubusercontent.com/2157285/62938132-a54ca400-bde3-11e9-8cf3-8437c388dd8a.png"> | **Multiplication** |
| `1 / 2` | <img width="108" src="https://user-images.githubusercontent.com/2157285/62937962-45ee9400-bde3-11e9-8262-e3c32e9b7617.png"> | **Division** |
| `1^2` | <img width="102" src="https://user-images.githubusercontent.com/2157285/62937963-45ee9400-bde3-11e9-88ee-7b640ef14cc0.png"> | **Power** |
| `1 % 2` | <img width="124" src="https://user-images.githubusercontent.com/2157285/62941253-dbd9ed00-bdea-11e9-8f69-b51afacf56a2.png"> | Modulo |
| `1 ? 1` | <img width="120" src="https://user-images.githubusercontent.com/2157285/62939696-1b9ed580-bde7-11e9-8566-d60143b04e3a.png"> | **Equals** <br> Returns 1 if equals and 0 otherwise |
| `1 ! 1` | <img width="117" src="https://user-images.githubusercontent.com/2157285/62939697-1b9ed580-bde7-11e9-8dd1-42783e23a6b2.png"> | **Not Equals** <br> Returns ` if not equals and 0 otherwise |


### Notational Functions
| Input | Representation | Description |
|:------|:---------------|:------------|
| `sqrt[1/2]` | <img width="150" src="https://user-images.githubusercontent.com/2157285/62938357-27d56380-bde4-11e9-8d10-e58004d5e2f5.png"> | Square Root |
| `abs[1/2]` | <img width="138" src="https://user-images.githubusercontent.com/2157285/62938360-29069080-bde4-11e9-9972-a99bdfbdc0cb.png"> | Absolute |
| `ceil[1/2]` | <img width="140" src="https://user-images.githubusercontent.com/2157285/62938361-29069080-bde4-11e9-8bdd-70096f14185c.png"> | Ceiling |
| `floor[1/2]` | <img width="138" src="https://user-images.githubusercontent.com/2157285/62938362-29069080-bde4-11e9-8069-8d681c0ada8d.png"> | Floor |
