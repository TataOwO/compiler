class Main {
    public static void main(String args[]) {
	String s1;
        String s2;

	boolean b = true;
	b = b && "toto".equals("toto");
	b = b && ! "toto".equals("toti");
	b = b && "toto".equals("to" + "to");
	b = b && ("to" + "to").equals("t" + "o" + "t" + "o");
	b = b && "1234".equals("" + 1 + 2 + 3 + 4);

	s1 = "toto";
	s2 = s1;
	b = b && "toto".equals(s1);
	b = b && s1.equals("toto");
	b = b && s1.equals(s2);

	s1 = "tito";
	b = b && ! s1.equals(s2);
	b = b && s1.equals("tito");
	b = b && ! s1.equals("toto");

	if (b)
	    System.out.print("ok\n");
    }
}
