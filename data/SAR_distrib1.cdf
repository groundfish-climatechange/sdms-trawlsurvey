netcdf SAR_distrib1 {
dimensions:
	t = UNLIMITED ; // (4 currently)
	b = 89 ;
variables:
	int t1(t) ;
		t1:long_name = "t1" ;
	double t(t) ;
		t:units = "seconds since 2013-01-01 00:00:00 +10" ;
		t:_FillValue = 0. ;
		t:dt = 86400. ;
	double SAR_distrib1(t, b) ;
		SAR_distrib1:units = " " ;
		SAR_distrib1:_FillValue = 0. ;

// global attributes:
		:title = "Spatial dists sardine juv" ;
		:geometry = "CalCurrentV3_utm.bgm" ;
		:parameters = "" ;
		:history = "Created as a test file" ;
data:

 t1 = 1, 2, 3, 4;

 t = 86400, 7862400, 15724800, 23587200;

 SAR_distrib1 =
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0005, 0.00084, 0.00127, 0.0022, 0.00444, 0.13511, 0.00146, 0.00193, 0.0014, 0.00214, 0.00441, 0.09181, 0.0006, 0.00075, 0.0004, 0.00481, 0.00835, 0.09468, 0.00101, 0.0008, 0.00075, 0.00308, 0.00044, 0, 0.00077, 0.00032, 0.00024, 0.01473, 0.02321, 0.02913, 0.18176, 0.00102, 0.00117, 0.00099, 0.00167, 0.00673, 0.16626, 0.00319, 0.00522, 0.00331, 0.00386, 0.00311, 0.18994, 0.00019, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0005, 0.00084, 0.00127, 0.0022, 0.00444, 0.13511, 0.00146, 0.00193, 0.0014, 0.00214, 0.00441, 0.09181, 0.0006, 0.00075, 0.0004, 0.00481, 0.00835, 0.09468, 0.00101, 0.0008, 0.00075, 0.00308, 0.00044, 0, 0.00077, 0.00032, 0.00024, 0.01473, 0.02321, 0.02913, 0.18176, 0.00102, 0.00117, 0.00099, 0.00167, 0.00673, 0.16626, 0.00319, 0.00522, 0.00331, 0.00386, 0.00311, 0.18994, 0.00019, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0005, 0.00084, 0.00127, 0.0022, 0.00444, 0.13511, 0.00146, 0.00193, 0.0014, 0.00214, 0.00441, 0.09181, 0.0006, 0.00075, 0.0004, 0.00481, 0.00835, 0.09468, 0.00101, 0.0008, 0.00075, 0.00308, 0.00044, 0, 0.00077, 0.00032, 0.00024, 0.01473, 0.02321, 0.02913, 0.18176, 0.00102, 0.00117, 0.00099, 0.00167, 0.00673, 0.16626, 0.00319, 0.00522, 0.00331, 0.00386, 0.00311, 0.18994, 0.00019, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0005, 0.00084, 0.00127, 0.0022, 0.00444, 0.13511, 0.00146, 0.00193, 0.0014, 0.00214, 0.00441, 0.09181, 0.0006, 0.00075, 0.0004, 0.00481, 0.00835, 0.09468, 0.00101, 0.0008, 0.00075, 0.00308, 0.00044, 0, 0.00077, 0.00032, 0.00024, 0.01473, 0.02321, 0.02913, 0.18176, 0.00102, 0.00117, 0.00099, 0.00167, 0.00673, 0.16626, 0.00319, 0.00522, 0.00331, 0.00386, 0.00311, 0.18994, 0.00019, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;

}