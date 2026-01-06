class Fact {
    Fact() {}
    int fact_imp(int n) {
	int res = 1;
	for ( ; n > 1; n=n-1)
	    res = res * n;
	return res;
    }
}
class Main {
    public static void main(String args[]) {
        Fact f = new Fact();
	if (f.fact_imp(5) == 120)
	    System.out.print("ok\n");
    }
}
