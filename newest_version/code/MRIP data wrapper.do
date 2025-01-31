
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data" //data stored here

/* Use these to control the years for which the MRIP data is polled/queried*/
global yearlist  1982 1983 1984 1985 1986 1987 1988 1989 ///
					  1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 ///
					   2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 ///
					   2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022				   

global wavelist 1 2 3 4 5 6

foreach year of global yearlist{
	foreach wave of global wavelist{

capture confirm file  "trip_`year'`wave'.dta"
if _rc==0{
	use trip_`year'`wave'.dta, clear
	renvarlab, lower
	save trip_`year'`wave'.dta, replace
}

else{
	
}

capture confirm file "size_b2_`year'`wave'.dta"
if _rc==0{
	use size_b2_`year'`wave'.dta, clear
	renvarlab, lower
	save size_b2_`year'`wave'.dta, replace
}

else{
	
}

capture confirm file "size_`year'`wave'.dta"
if _rc==0{
	use size_`year'`wave'.dta, clear
	renvarlab, lower
	save size_`year'`wave'.dta, replace
}

else{
	
}

capture confirm file "catch_`year'`wave'.dta"
if _rc==0{
	use catch_`year'`wave'.dta, clear
	renvarlab, lower
	save catch_`year'`wave'.dta, replace
}

else{
	
}

}
}

set matsize 10000


/*catchlist -- this assembles then names of files that are needed in the catchlist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global catchlist
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "catch_`year'`wave'.dta"
	if _rc==0{
		use "catch_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global catchlist "$catchlist "catch_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/*Triplist -- this assembles then names of files that are needed in the Triplist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global triplist
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "trip_`year'`wave'.dta"
	if _rc==0{
		use "trip_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global triplist "$triplist "trip_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/*B2 Files*/
global b2list
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "size_b2_`year'`wave'.dta"
	if _rc==0{
		use "size_b2_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global b2list "$b2list "size_b2_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}



/*SIZE_LIST */
global sizelist
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "size_`year'`wave'.dta"
	if _rc==0{
	use "size_`year'`wave'.dta", clear
	quietly count
	scalar tt=r(N)
	if scalar(tt)>0{
		global sizelist "$sizelist "size_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}