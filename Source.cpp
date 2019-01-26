#include <iostream>
#include "Hungarian.h"
#include <cassert>


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

	return 0;
}

