#include <ctime>
#include <cassert>
#include <iostream>
#include "Hungarian.h"
using namespace std;

vector< vector<double> > randMat(int n) {

	vector< vector<double> > mat;

	for (size_t i = 0; i < n; i++)
	{
		vector<double> row = {};
		for (size_t j = 0; j < n; j++)
			row.insert(row.begin(), ((double)rand() / RAND_MAX)*n);
		mat.insert(mat.begin(), row);
	}
	return mat;
}

int solve(vector< vector<double> > costMatrix ) {
	HungarianAlgorithm HungAlgo;
	vector<int> assignment;
	double cost = HungAlgo.Solve(costMatrix, assignment);
	std::cout << "\ncost: " << cost << std::endl;
	return 0;
}

int test(vector< vector<double> > costMatrix, double expected) {
	HungarianAlgorithm HungAlgo;
	vector<int> assignment;

	double cost = HungAlgo.Solve(costMatrix, assignment);

	for (unsigned int x = 0; x < costMatrix.size(); x++)
		std::cout << x << "," << assignment[x] << "\t";

	std::cout << "\ncost: " << cost << std::endl;
	assert(cost == expected);
	return 0;
}

int main(void)
{
	clock_t time_req;
	test({ 
			{4, 3, 2, 1},
			{6, 6, 5, 4},
			{1, 9, 8, 7},
			{3, 2, 1, 1}
		}, 9);

	test({
			{400, 150, 400},
			{400, 450, 600},
			{300, 225, 300} 
		}, 850);

		// Rectangular variant
	test({ 
			{400, 150, 400, 1},
			{400, 450, 600, 2},
			{300, 225, 300, 3}
		}, 452);

	// Square
	test({ 
			{10, 10, 8},
			{9, 8, 1 },
			{9, 7, 4} 
		}, 18);

		// Rectangular variant
	test({ 
			{10, 10, 8, 11},
			{9, 8, 1, 1},
			{9, 7, 4, 10}
		}, 15);
	for (size_t i = 1; i <= 10; i++)
	{
		vector<vector<double>> mat = randMat(i*100);
		time_req = clock();
		solve(mat);
		time_req = clock() - time_req;
		cout  <<"\n"<< i*100 << "X" << i*100 << " matrix time: " << (float)time_req / (CLOCKS_PER_SEC / 1000) << " ms.\n" << endl;
	} 
	
	
	
	return 0;
}
