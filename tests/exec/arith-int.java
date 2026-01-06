class Main {
    public static void main(String args[]) { 
	int expr1 =- 7 + 6 * 5 - 4 / 3 - 2;     // 20
	int expr2 = (7 + 6) * (5 - 4) / 3 - 2; // 2
	int expr3 = 7 + 6 * (5 - 4 / (3 - 2));  // 13

	int sum = expr1 % 8 + expr2 + expr3;    // 19

	if (sum == 19) System.out.print("ok\n");
    }
}
