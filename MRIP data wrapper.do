

cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"



/*foreach wave in 20171 20172 20173 20174 20175 20176 ///
						20181 20182 20183 20184 20185 20186 ///
						20191 20192 20193 20194 20195 20196 ///
						20201 20202 20203 20204 20205 20206 ///
						20211 20212 20213 20214 20215 20216 ///
						20221 20222 20223 20224 20225 20226  /// */
	foreach wave in	19822 19823 19824 19825 19826 {	///
/*	   ///
						19831 19832 19833 19834 19835 19836 ///
						19841 19842 19843 19844 19845 19846 ///
						19851 19852 19853 19854 19855 19856 ///
						19861 19862 19863 19864 19865 19866 ///
						19871 19872 19873 19874 19875 19876 ///					
						19881 19882 19883 19884 19885 19886 ///						
						19891 19892 19893 19894 19895 19896 ///						
						19901 19902 19903 19904 19905 19906 ///						
						19911 19912 19913 19914 19915 19916 ///						
						19921 19922 19923 19924 19925 19926 ///
						19931 19932 19933 19934 19935 19936 ///						
						19941 19942 19943 19944 19945 19946 ///						
						19951 19952 19953 19954 19955 19956 ///
						19961 19962 19963 19964 19965 19966 ///						
						19971 19972 19973 19974 19975 19976 ///						
						19981 19982 19983 19984 19985 19986 ///
						19991 19992 19993 19994 19995 19996 ///						
						20001 20002 20003 20004 20005 20006 ///
						20011 20012 20013 20014 20015 20016 ///
						20021 20022 20023 20024 20025 20026 ///
						20031 20032 20033 20034 20035 20036 /// 
						20041 20042 20043 20044 20045 20046 ///
						20051 20052 20053 20054 20055 20056 ///
						20061 20062 20063 20064 20065 20066 /// 
						20071 20072 20073 20074 20075 20076 ///
						20081 20082 20083 20084 20085 20086 ///
						20091 20092 20093 20094 20095 20096 ///
						20101 20102 20103 20104 20105 20106 ///
						20111 20112 20113 20114 20115 20116 ///
						20121 20122 20123 20124 20125 20126 /// 
						20131 20132 20133 20134 20135 20136 ///
						20141 20142 20143 20144 20145 20146 ///
						20151 20152 20153 20154 20155 20156 /// 
						20161 20162 20163 20164 20165 20166 ///
						20171 20172 20173 20174 20175 20176 ///
						20181 20182 20183 20184 20185 20186 /// 
						20191 20192 20193 20194 20195 20196 ///
						20201 20202 20203 20204 20205 20206 ///
						*/
						*20211 20212 20213 20214 20215 20216{
						

capture confirm file  "trip_`wave'.dta"
if _rc==0{
	use trip_`wave'.dta, clear
	renvarlab, lower
	save trip_`wave'.dta, replace
}

else{
	
}

capture confirm file "size_b2_`wave'.dta"
if _rc==0{
	use size_b2_`wave'.dta, clear
	renvarlab, lower
	save size_b2_`wave'.dta, replace
}

else{
	
}

capture confirm file "size_`wave'.dta"
if _rc==0{
	use size_`wave'.dta, clear
	renvarlab, lower
	save size_`wave'.dta, replace
}

else{
	
}

capture confirm file "catch_`wave'.dta"
if _rc==0{
	use catch_`wave'..dta, clear
	renvarlab, lower
	save catch_`wave'.dta, replace
}

else{
	
}

}

set matsize 10000
/* Use these to control the years for which the MRIP data is polled/queried*/

global yearlist  1982 1983 1984 1985 1986 1987 1988 1989 ///
					  1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 ///
					   2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 ///
					   2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022
global yearlist 2010  2011 2012 2013 2014 2015 2016 2017 2018 2019 2020
*global yearlist 2019 2020					   
*global wavelist 1 2 3 4 5 6

global wavelist 1 2 3 4 5 6


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