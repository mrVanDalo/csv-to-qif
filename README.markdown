# CSV To Qif

transform a wide range of csv files to qif files, with the main target to import them into [GnuCash](http://www.gnucash.org/).

# Usage

You have to give `csv-to-qif` a lot of parameters, so it's best to create a bash script for every type of csv your bank will give you.

    #!/bin/bash
    
    FILE=$1
    NEW_FILE=/tmp/bank.tmp
    CONFIG=parse.conf
    OUTPUT="Active:Bank-Name:CreditCard.qif"
    SKIP=7
    
    iconv -t UTF-8 -f ISO-8859-15 $FILE > $NEW_FILE
    
    csv-to-qif --input=$NEW_FILE --output=$OUTPUT \
        --date=0 --text=3,4 --longtext=3,4,5 --balance=7 \
        --skip=$SKIP --separator=';' --updater=$CONFIG
    
    head -n $SKIP $FILE

## Parameters

### Mandatory

* input : csv input file
* output : qif file name to write to.
* date : column of date of transaction
* text : list of columns to create description from
* longtext : same like text but for more information
* balance : column of balance of transaction

### optional

* skip : how many lines to skip before reading
* separator : default is `,`  it must be a char!

### updater Parameter

Because some banks create descriptions that are _to detailed_.
You can create a updater configuration that will match for Strings and replaces the description (not the long description) with a replacement.
The format is `match`<->`replacement`

Example updater file.

    PayPal<->Bought Stuff at paypal
    Your small shop says Thank you<->Bought stuff at Shop

A file with this content

    "11.11.2011","Transaction to PayPal","Bottle of water","-1000.00"
    "12.11.2011","Your small","Your small shop says Thank you for buying stuff like Explosives","-200000.00"

parsed without updater

    $> csv-to-qif --input=foo.csv --output=foo.qif --date=0 --text=1,2 --longtext=1,2,3 --balance=3
    $> cat foo.qif

    !Type:Bank
    PTransaction to PayPal Bottle of water
    T-1000.00
    D11.11.2011
    MTransaction to PayPal Bottle of water -1000.00
    ^
    PYour small Your small shop says Thank you for buying stuff like Explosives
    T-200000.00
    D12.11.2011
    MYour small Your small shop says Thank you for buying stuff like Explosives -200000.00
    ^
parsed with updater

    $> csv-to-qif --input=foo.csv --output=foo-updated.qif --date=0 --text=1,2 --longtext=1,2,3 --balance=3 --updater=updater.conf
    $> cat foo-updated.qif

    !Type:Bank
    PBought Stuff at paypal
    T-1000.00
    D11.11.2011
    MTransaction to PayPal Bottle of water -1000.00
    ^
    PBought stuff at Shop
    T-200000.00
    D12.11.2011
    MYour small Your small shop says Thank you for buying stuff like Explosives -200000.00
    ^

    

# Install

## using Cabal

    $> cabal update
    $> cabal install csv-to-qif

should do it.

## from sources

get the source

    $> git clone  https://github.com/mrVanDalo/csv-to-qif.git
    $> cd csv-to-qif

install from source

    $> cabal clean                      # just to be sure
    $> cabal configure --enable-tests   # we want also the tests
    $> cabal build                      # sure we want to build
    $> cabal test && cabal install      # only install when tests are ok



