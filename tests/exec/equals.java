class A {
    String s;

    A(String s) { this.s = s;}

    boolean equals(String s) { return this.s.equals(s); }
}

class Main {
    public static void main(String args[]) {
	if (new A("toto").equals("toto") && ! new A("titi").equals("toto"))
	    System.out.print("ok\n");
    }
}
