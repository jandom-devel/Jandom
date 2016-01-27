// Example 'h' in the paper "G. Amato, F. Scozzari. Localizing widening and narrowing. SAS 2013",
// first appeared in "N. Halbwachs and J. Henry. When the decreasing sequence fail. SAS 2012".
// This example show the improvement it is possible to obtain through the use of localized 
// widening (it requires a relational domain).

model amato_scozzari_sas13_hh {

	var i, j;	
	states s1, s2, s3;
	
	transition t1 := {
		from := s1;
		to := s2;
		guard := i < 4;
		action := j' = 0;
	};
	
	transition t2 := {
		from := s2;
		to := s2;
		guard := j < 4;
		action := i' = i + 1, j' = j + 1;
	};
	
	transition t3 := {
		from := s2;
		to := s1;
		guard := j >= 4;
		action := i' = i - j + 1;
	};
}

strategy s {
	Region init := { state = s1 && i = 0 };
}
