class A { A () { } }

class Main {
    public static void main(String args[]) {
	A a = new A();

	boolean b = true;
	b = b && null == null;
	b = b && null != (Object)a;
	b = b && null != "toto";
	b = b && null != a;
	a = null;
	b = b && null == a;

	if (b)
	    System.out.print("ok\n");
    }
}
