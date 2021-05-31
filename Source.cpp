#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <map>
#include <utility>
#include <fstream>
#include <sstream>
#include <algorithm>
using namespace std;

struct instruction {
	int rd = 0;
	int rs1 = 0;
	int rs2 = 0;
	int immediate = 0;
	string type = "";
	int  type_int = 0;
	string instruction_ = "";
	int  issue_cycle = 0;
	int Starting_Execution_Cycle = 0;
	int Ending_Execution_Cycle = 0;
	int Write_Back_Cycle = 0;
	int operand_ = 0;
	int status[3] = {};
};

instruction instruction_parsing(string assembly_line);


vector<instruction> parse(string filename) {
	//ifstream instructions;
	ifstream instructions(filename);
	//	instructions.open("assembly_code.txt");

	if (!instructions) { // file couldn't be opened
		cerr << "Error: file could not be opened" << endl;
		exit(1);
	}
	vector<instruction> inst;
	string data;
	while (getline(instructions, data)) {

		inst.push_back(instruction_parsing(data));

	}
	return inst;
}




vector<vector<int>> tomasulo(vector<instruction> inst, unordered_map<int, int> &mem, int cycles[], int RS[], int PC, double &branch_mispred) {

	vector<int> rFile(8, 0);
	int clk = 1;
	int RS_busy[7];
	int ret_addr = 0;
	int branches = 0, taken_branch = 0;
	vector<vector<int>> RStations(7);

	for (int i = 0; i < 7; i++) {
		RStations[i].resize(RS[i], 0);
	}
	unordered_map<int, int> reg_ready;
	int n = inst.size();

	vector<vector<int>> table;

	for (int i = 0; i < 7; i++) {
		RS_busy[i] = 0;
	}

	int temp1 = 0, temp2 = 0;
	int temp_ind = 0;
	int br_addr = 0;
	bool b = false;

	for (int i = 0; i < n; i++) {
		

		vector<int> tt;
		//vector<int> tt1;
		temp1 = clk;
		temp2 = clk;
		tt.push_back(i); //PC

	//issue
		if (RS_busy[inst[i].type_int] < RS[inst[i].type_int] && reg_ready[inst[i].rd] < clk) {
			RS_busy[inst[i].type_int]++;
			tt.push_back(clk);
		}
		else {
			//check RS free
			if (RS_busy[inst[i].type_int] == RS[inst[i].type_int]) {
				temp1 = RStations[inst[i].type_int][0];
				for (int j = 1; j < RS[inst[i].type_int]; j++) {
					temp1 = min(temp1, RStations[inst[i].type_int][j]);
				}
			}

			//check WAW hazard
			if (inst[i].rd > 0 && reg_ready.count(inst[i].rd) && reg_ready[inst[i].rd] + 1 >= clk) {
				temp2 = reg_ready[inst[i].rd] + 2;
			}

			clk = max(temp1 + 1, temp2);
			tt.push_back(clk); //issue cycle
		}

		if (b) {
			tt.push_back(0);
			tt.push_back(0);
			tt.push_back(0);
			table.push_back(tt);
			clk++;
			rFile[0] = 0;
			i = br_addr;
			b = false;
			continue;
		}

		//execute
			//check rs1, rs2 ready or not
		temp1 = 0;
		if (inst[i].rs1 > 0 && reg_ready.count(inst[i].rs1))
			temp1 = reg_ready[inst[i].rs1];
		if (inst[i].rs2 > 0 && reg_ready.count(inst[i].rs2))
			temp1 = max(temp1, reg_ready[inst[i].rs2]);

		tt.push_back(max(clk + 1, temp1 + 1)); //exec start cycle

		tt.push_back(tt[2] + cycles[inst[i].type_int] - 1);

		reg_ready[inst[i].rd] = tt[3];

		//write back
		tt.push_back(tt[3] + 1);

		//update RStations
		temp1 = INT_MAX;
		for (int j = 0; j < RStations[inst[i].type_int].size(); j++) {
			if (RStations[inst[i].type_int][j] < temp1) {
				temp1 = RStations[inst[i].type_int][j];
				temp_ind = j;
			}
		}
		RStations[inst[i].type_int][temp_ind] = tt[3];

		if (inst[i].type_int == 4 || inst[i].type_int == 3) {
			clk = tt[3] - 1;
		}

		//update reg/mem // branch
		if (inst[i].type == "LW") {
			rFile[inst[i].rd] = mem[(inst[i].immediate + rFile[inst[i].rs1])];
		}
		else if (inst[i].type == "SW") {
			mem[(inst[i].immediate + rFile[inst[i].rs1])] = rFile[inst[i].rs2];
		}
		else if (inst[i].type == "BEQ") {
			branches++;
			if (rFile[inst[i].rs1] == rFile[inst[i].rs2]) {
				b = true;
				br_addr = i + inst[i].immediate - PC - 1;
				taken_branch++;
			}
		}
		else if (inst[i].type == "ADD") {
			rFile[inst[i].rd] = rFile[inst[i].rs1] + rFile[inst[i].rs2];
		}
		else if (inst[i].type == "ADDI") {
			rFile[inst[i].rd] = rFile[inst[i].rs1] + inst[i].immediate;
		}
		else if (inst[i].type == "NEG") {
			rFile[inst[i].rd] = ~rFile[inst[i].rs1];
		}
		else if (inst[i].type == "JALR") {
			//cout << ret_addr << endl << rFile[inst[i].rs1] << endl << inst[i].rs1 << endl;
			rFile[1] = i;
			i = rFile[inst[i].rs1] - 1;
		}
		else if (inst[i].type == "RET") {
			i = rFile[1] - 1;
			//if(ret_addr!=-1)i = ret_addr;
			//ret_addr = -1;
		}
		else if (inst[i].type == "MULL") {
			rFile[inst[i].rd] = (long long)(rFile[inst[i].rs1] * rFile[inst[i].rs2]) & 0xFFFF;
		}
		else if (inst[i].type == "MULH") {
			rFile[inst[i].rd] = (long long)(rFile[inst[i].rs1] * rFile[inst[i].rs2]) >> 16;
		}
		clk++;
		
		table.push_back(tt);

		rFile[0] = 0;
	}
	if (branches)
		branch_mispred = (double)taken_branch / (double)branches;
	return table;
}


int main() {

	//parse file
	vector<instruction> Inst;
	Inst = parse("C:/Users/Nada/Desktop/assembly_code.txt");


	//user input
	int Cycle[7], RS_[7];
	RS_[0] = Cycle[0] = 0;

	cout << "Enter amount of RS stations for [LW,SW,BEQ,JALR,ADD,MULL] respectively:\n";
	for (int i = 1; i < 7; i++)
		cin >> RS_[i];

	cout << "Enter amount of cycles for [LW,SW,BEQ,JALR,ADD,MULL] respectively:\n";
	for (int i = 1; i < 7; i++)
		cin >> Cycle[i];

	int PC;
	cout << "Enter your starting address:\n";
	cin >> PC;

	int x;
	cout << "Number of value to add to the data memory: ";
	cin >> x;

	int addr = 0, value = 0;

	unordered_map<int, int>memory;
	for (int i = 0; i < x; ++i) {
		cout << "Address" << i << ": ";
		cin >> addr;
		cout << "Value" << i << ": ";
		cin >> value;
		memory[addr] = value;
	}

	//call tomasulo
	vector<vector<int>> arr;
	double branch_mispred = 0;
	arr = tomasulo(Inst, memory, Cycle, RS_, PC, branch_mispred);

	cout << "\nINST                    Issue          Exec start          Exec end          Write back\n";
	for (int i = 0; i < arr.size(); i++) {
		cout << Inst[arr[i][0]].instruction_;
		
		for (int x = 0; x < 30 - Inst[arr[i][0]].instruction_.length(); x++) {
			cout << " ";
		}	
		cout << arr[i][1] << "                "<< arr[i][2] << "              " << arr[i][3] << "                " << arr[i][4] << endl;
	}


	cout << "\nBranch mispredictions: " << branch_mispred << endl;
	cout << "Executed in " << arr[arr.size() - 1][4] << " clock cycles.\n";
	cout << "IPC: " << ((double)(arr.size())) / arr[arr.size() - 1][4];
	return 0;
}

instruction instruction_parsing(string assembly_line) {
	instruction instr;
	// parse the string (rd,rs1,rs2,immediate,type,operand)

	string temp = assembly_line;
	string type;
	stringstream ss(temp);
	ss >> type;
	
	//LW, SW, BEQ, JALR, RET, ADD, NEG, ADDI, MULL, MULH

	// Convert to int 

	if (type == "LW") instr.type_int = 1;
	else if (type == "SW") instr.type_int = 2;
	else if (type == "BEQ") instr.type_int = 3;
	else if (type == "JALR" || type == "RET") instr.type_int = 4;
	else if (type == "ADD" || type == "NEG" || type == "ADDI") instr.type_int = 5;
	else if (type == "MULL" || type == "MULH") instr.type_int = 6;


	if (type == "LW")
	{
		instr.rs2 = -1;
		string word;
		string h = "";
		while (ss >> word)
		{
			if (word != "LW")   h += word;
		}
		size_t  found_comma = h.find(",");
		size_t  found_open_bracket = h.find("(");
		instr.rd = stoi(h.substr(1, found_comma - 1));
		instr.immediate = stoi(h.substr(found_comma + 1, found_open_bracket - found_comma - 1));
		instr.rs1 = stoi(h.substr(found_open_bracket + 2, h.length() - found_open_bracket - 3));
	}
	else if (type == "SW")
	{
		instr.rd = -1;

		string word;
		string h = "";
		while (ss >> word)
		{
			if (word != "SW")   h += word;
		}
		size_t  found_comma = h.find(",");
		size_t  found_open_bracket = h.find("(");
		instr.rs2 = stoi(h.substr(1, found_comma - 1)); // X0 -> need 0 
		instr.immediate = stoi(h.substr(found_comma + 1, found_open_bracket - found_comma - 1)); // always a number
		instr.rs1 = stoi(h.substr(found_open_bracket + 2, h.length() - found_open_bracket - 3)); // +2 as we need the number
	}
	else if (type == "BEQ")
	{
		/* different occurences
								BEQ rs1, rs2, imm
			*/
		string word;
		string h = "";
		while (ss >> word)
		{
			if (word != "LW")   h += word;
		}

		size_t  found_1st_comma = h.find(",");
		size_t  found_2nd_comma = h.find(",", found_1st_comma + 1);

		instr.rs1 = stoi(h.substr(1, found_1st_comma - 1));
		instr.rs2 = stoi(h.substr(found_1st_comma + 2, found_2nd_comma - found_1st_comma - 2));
		instr.immediate = stoi(h.substr(found_2nd_comma + 1, h.length() - found_2nd_comma));

	}
	else if (type == "JALR")
	{
		instr.rd = 1;
		instr.rs2 = -1;
		string word;
		ss >> word;
		instr.rs1 = stoi(word.substr(1, word.length() - 1));

	}
	else if (type == "RET")
	{
		instr.rd = instr.rs2 = -1;
		instr.rs1 = 1;
	}

	else if (type == "ADD" || type == "MULL" || type == "MULH")
	{
		//MULL rd, rs1, rs2
	
		if (type == "ADD") instr.operand_ = 1;
		else if (type == "MULL") instr.operand_ = 2;
		else instr.operand_ = 3;


		string word;
		string h = "";
		while (ss >> word)
		{
			if (word != "MULL")   h += word;
		}

		size_t  found_1st_comma = h.find(",");
		size_t  found_2nd_comma = h.find(",", found_1st_comma + 1);

		instr.rd = stoi(h.substr(1, found_1st_comma - 1));
		instr.rs1 = stoi(h.substr(found_1st_comma + 2, found_2nd_comma - found_1st_comma - 2));
		instr.rs2 = stoi(h.substr(found_2nd_comma + 2, h.length() - found_2nd_comma));
	}

	else if (type == "ADDI")
	{   
		//ADDI rd, rs1, imm
		
		instr.rs2 = -1;
		instr.operand_ = 1;
		string word;
		string h = "";
		while (ss >> word)
		{
			if (word != "LW")   h += word;
		}

		size_t  found_1st_comma = h.find(",");
		size_t  found_2nd_comma = h.find(",", found_1st_comma + 1);

		instr.rd = stoi(h.substr(1, found_1st_comma - 1));
		instr.rs1 = stoi(h.substr(found_1st_comma + 2, found_2nd_comma - found_1st_comma - 2));
		instr.immediate = stoi(h.substr(found_2nd_comma + 1, h.length() - found_2nd_comma));
	}
	else if (type == "NEG")
	{
		instr.rs2 = -1;
		instr.operand_ = 4;
		string word;
		string h = "";
		while (ss >> word)
		{
			if (word != "LW")   h += word;
		}

		size_t  found_1st_comma = h.find(",");
		instr.rd = stoi(h.substr(1, found_1st_comma - 1));
		instr.rs1 = stoi(h.substr(found_1st_comma + 2, h.length() - found_1st_comma));
	}

	instr.instruction_ = assembly_line;
	instr.type = type;
	return instr;
}
