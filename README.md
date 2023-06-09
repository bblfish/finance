# Coalgebraic Finance Calculator

[![scratchanitch.dev badge](https://img.shields.io/badge/scratchanitch-dev-FFC4B5)](https://scratchanitch.dev)

## Index

* [About](#about)
* [Change Model](#the-csv-change-model)
* [Running the Model](#running-the-model)
* [Todo](#todo)

## About


This little finance calculator takes a CSV file of income or expenditures Orders and builds a Lazy List of infinite unfolding Account States, each one month apart, generated by adding income and removing expenses from the account as specified by the orders.

One can evaluate the quality of the model by starting at a time in the past and checking if it arrives at the right sum in the present. If the model arrives at the right conclusions (for the right reasons), one can then use it to project the state of one's account into the future.

The model of change is of a stream of changes, with as functions between given by the orders. The final coalgebra of these change functions gives the infinite stream (here a LazyList, which used to be called a Stream)

At this point all the code holds in one file [src/main/scala/Main.scala](src/main/scala/Main.scala).
The code in this repository is published under the Apache 2.0 License.

## The CSV change model

An example [data.csv](data/data.csv) file is available.
It consists of 5 columns

* **Topic**: short name (line item?)
* **Amount**: a positive or negative floating point giving the incoming or outgoing value
* **Repeat**: How often that is repeated: currently only `M` or `Month` for monthly repetitions (Daily or Yearly are also
  possible), or a date in `yyyy/mm/dd` format for a one-time payment.
* **Start**: Optional Start date in `yyyy/mm/dd` format
* **Ending**: Optional Ending date in `yyyy/mm/dd` format
* **Description**: more detailed descriptive information

Here is an example:

```csv
Topic:Amount:Repeat:Start:Ending:Description
Salary:2400:M:::Monthly Salary
WebServers:-19:M:::Scaleway
Rent:-800:Monthly:::
Electricity:-80:M:::
Internet:-50:M:::5G
```

## Running the model

One can run the code from inside `sbt`, which can be installed along with other scala tools
a number of ways as explained on the [Scala Downloads page](https://www.scala-lang.org/download/) page.
Invoking `run` gives a list of command line arguments.

```commandline
$ sbt
sbt:finance-model> run
[info] running run
Error: Missing argument <file>
accounts 0.1
Usage: accounts [options] <file>

  <file>                file containing csv data in required format
  -a, --amount <value>  starting value in account
  -s, --start <value>   date in yyyy/mm/dd format to start from
  -m, --months <value>  months from start date to display           oo
```

We can then calculate 1 month starting with 159 in the bank from
August 2022. This adds the incoming and outgoins and lists each on a row. But note that it does not add the laptop
lease.

```sbt
sbt:finance-model> run data/data.csv -s 2022/08/25 -a 159 -m 1
[info] running run data/data.csv -s 2022/08/25 -a 159 -m 1
2022-08-25: remaining: € 159.0
     out: € -0.0
     in : € 0.0

2022-09-25: remaining: € 1160.0
     out: € 1399.0
     in : € 2400.0
   Salary:	 € 2400.0 Month	Monthly Salary
   WebServers:	 € -19.0 Month	Scaleway
   Rent:	 € -800.0 Month	
   Electricity:	 € -80.0 Month	
   Internet:	 € -50.0 Month	5G
   Insurance:	 € -50.0 Month	Legal insurance
   Food:	 € -400.0 Month	


TOTAL
=====
income: 2400.0 average: 2400.0 over 1 months
expend: -1399.0 average: -1399.0
```

We can then look at what happens over 3 months, allowing
us to see that if these were all the expenses, we should
have put 2700 aside.

```sbt
sbt:finance-model> run data/data.csv -s 2022/08/25 -a 159 -m 3
[info] running run data/data.csv -s 2022/08/25 -a 159 -m 3
2022-08-25: remaining: € 159.0
     out: € -0.0
     in : € 0.0

2022-09-25: remaining: € 1160.0
     out: € 1399.0
     in : € 2400.0
   Salary:	 € 2400.0 Month	Monthly Salary
   WebServers:	 € -19.0 Month	Scaleway
   Rent:	 € -800.0 Month	
   Electricity:	 € -80.0 Month	
   Internet:	 € -50.0 Month	5G
   Insurance:	 € -50.0 Month	Legal insurance
   Food:	 € -400.0 Month	
2022-10-25: remaining: € 1921.2
     out: € 1638.8000030517578
     in : € 2400.0
   Salary:	 € 2400.0 Month	Monthly Salary
   WebServers:	 € -19.0 Month	Scaleway
   Rent:	 € -800.0 Month	
   Electricity:	 € -80.0 Month	
   Internet:	 € -50.0 Month	5G
   Insurance:	 € -50.0 Month	Legal insurance
   Food:	 € -400.0 Month	
   Amazon:	 € -65.0 Once(2022-09-27)	Kites
   Leasing Mac:	 € -174.8 Month	Mac Book M1 Pro Max Leased
2022-11-25: remaining: € 2747.4001
     out: € 1573.8000030517578
     in : € 2400.0
   Salary:	 € 2400.0 Month	Monthly Salary
   WebServers:	 € -19.0 Month	Scaleway
   Rent:	 € -800.0 Month	
   Electricity:	 € -80.0 Month	
   Internet:	 € -50.0 Month	5G
   Insurance:	 € -50.0 Month	Legal insurance
   Food:	 € -400.0 Month	
   Leasing Mac:	 € -174.8 Month	Mac Book M1 Pro Max Leased


TOTAL
=====
income: 7200.0 average: 2400.0 over 3 months
expend: -4611.5996 average: -1537.1998
```

## TODO

There are many things one could do.

One simple one is that a Stream is a Comonad, and comanads are very useful to calculate changes in the state between past and future. So one could calulate local minima, etc...

