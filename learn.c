#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "learn.h"

int col;
int row;
int augCol;
//double **result;


void printMatrix(double**mx, int rowLocal, int colLocal){
	int cnt, cnt2;
	for(cnt = 0; cnt < rowLocal; cnt++){
		for(cnt2 = 0; cnt2 < colLocal; cnt2++){
			if(cnt2 + 1 == colLocal)
				printf("%lf", mx[cnt][cnt2]);
			else
				printf("%lf,\t", mx[cnt][cnt2]);		
		}
		printf("\n");
	}
}


void printVector(double*v, int row){
	int cnt;
	for(cnt = 0; cnt < row; cnt++){
		printf("%lf\n", v[cnt]);
	}
}


// finding the transpose of a matrix
double ** transpose(double **mx){
	double **T = (double **)malloc(col * sizeof(double*));	

	int cnt;
	for(cnt = 0; cnt < col; cnt++){
		T[cnt] = (double*)malloc(row * sizeof(double));
	}


	int i,j;
	for(j = 0; j < col; j++){
		
		for(i = 0; i < row; i++){
			T[j][i] = mx[i][j];
		}

	}
	////printMatrix(T,col,row);
	return T;
}


// multiply a matrix by a vector
double * matrixXVector(double **mx, double* v, int row, int col){
	double *result = (double *)malloc(row * sizeof(double));
	int cnt, cnt2;
	double sum;
	//int counter = 0;
	for(cnt = 0; cnt < row;cnt++){
		sum = 0;
		for(cnt2 = 0; cnt2 < col; cnt2++){
			sum = sum + (mx[cnt][cnt2] * v[cnt2]);
		}
		result[cnt] = sum;
	}
	//printVector(result,row);
	return result;
}


// multiply a matrix by a matrix
double** matrixXMatrix(double **mx1, double**mx2, int row, int col, int same){
	double ** result = (double **)malloc(row * sizeof(double*));

	int cnt,cnt2,cnt3;
	double sum;
	//int num;
	
	//printf("row = %d\n",row);
	//printf("col = %d\n",col);
	//printf("same = %d\n",same);

	for(cnt = 0; cnt < row; cnt++){
		result[cnt] = (double*)malloc(col * sizeof(double));
	}

	for(cnt = 0; cnt < row; cnt++){
		for(cnt2 = 0; cnt2 < col; cnt2++){
			sum = 0;
			for(cnt3 = 0; cnt3 < same; cnt3++){
				sum = sum + (mx1[cnt][cnt3] * mx2[cnt3][cnt2]);
			}
			//num++;
			//printf("sum = %lf\n",sum);
			result[cnt][cnt2] = sum;
		}
	}
	//printf("number = %d",num);
	//printMatrix(result,row,col);
	return result;
}


// combining matrix with identity matrix
double ** augmentedMatrix(double **mx, int row){
	double ** result = (double **)malloc(row * sizeof(double*));	

	int cnt,cnt2;
	augCol = row * 2;
	for(cnt = 0; cnt < row; cnt++){
		result[cnt] = (double*)malloc(augCol * sizeof(double));
	}

		
	// inputting original matrix into new and give rest 0 value
	for(cnt = 0; cnt < row; cnt++){
		for(cnt2 = 0; cnt2 < augCol; cnt2++){			
			if(cnt2 < row)
				result[cnt][cnt2] = mx[cnt][cnt2];
			else
				result[cnt][cnt2] = 0;
		}		
	}
	
	int oneIndex = 0;

	// inserting 1 into diagonal
	for(cnt = 0; cnt < row; cnt++){
		result[cnt][row + oneIndex] = 1;
		oneIndex++;	
	}
	

	////printf("\n");
	//printMatrix(result,row,augCol);
	return result;
}


double ** divideItself(double** mx, int r, double constant){
	int cnt;
	double ** result = mx;
	for(cnt = 0; cnt < augCol; cnt++)
		result[r][cnt] = result[r][cnt] / constant;
	return result;
}

double ** setPivotZeroToOne(double** mx, int r1, int r2, double constant){
	int cnt = 0;
	double ** result = mx;
	for(cnt = 0; cnt < augCol; cnt++)
		result[r1][cnt] = result[r1][cnt] + (constant*result[r2][cnt]);
	return result;
}

double ** subtract(double** mx, int r1, int r2, double constant){
	int cnt;
	double ** result = mx;
	for(cnt = 0; cnt < augCol; cnt++)
		result[r1][cnt] = result[r1][cnt] - (constant*result[r2][cnt]);
	return result;
}


double ** subtractReduced(double** mx, int r1, int r2, double constant){
	int cnt;
	double ** result = mx;
	for(cnt = 0; cnt < augCol; cnt++)
		result[r1][cnt+r2] = result[r1][cnt+r2] - (constant*result[r2][cnt + r2]);
	return result;
}

// finding row echelon form
double** ref(double** mx, int row){
	double ** result = mx;
	double helper;
	int cnt, cnt2, index;
	//int stop = col;
	
	//if(col > row)
		//stop = col - row;

	for(cnt = 0; cnt < row; cnt++){	// loop through each columns
		if(result[cnt][cnt] == 0){	// use another row to help
			for(index = 1; (index + cnt) < row; index++){	// loop to find a non-zero row in that col
				if(result[cnt+index][cnt] != 0){
					helper = 1 / (result[cnt+index][cnt]);
					break;
				}
			}
			//printMatrix(result,row,augCol);
			//printf("Values passing through\nr1 = %d, r2 = %d, constant = %lf\n",cnt,cnt+index,helper);
			result = setPivotZeroToOne(result,cnt,cnt+index,helper);
		}
		else if(result[cnt][cnt] != 1){	// divide itself (pivot) to make it 1
			result = divideItself(result,cnt,result[cnt][cnt]);
		}
	//	printf("\n\nMade pivot = 1\n");
	//	printMatrix(result,col,augCol);
		//printf("\n");

		// start making everything under pivot to become 0
		for(cnt2 = 1; cnt2 < row-cnt; cnt2++){	// loop through each rows of that column to set to 0
			result = subtract(result,cnt+cnt2,cnt,result[cnt+cnt2][cnt]);
			//printMatrix(result,col,augCol);
			//printf("\n");
		}
	}
	//printMatrix(result,row,augCol);
	return result;
}


// finding reduced row echelon form
double ** rref(double** mx, int row){
	double ** result = mx;
	int cnt, cnt2;

//	printf("\n\nHi\n");
//	printMatrix(result,col,augCol);
	///printf("\n");
	

	for(cnt = 1; cnt < row; cnt++){
		for(cnt2 = 1; (cnt - cnt2) >= 0; cnt2++){
			//printf("cnt = %d cnt2 = %d\n",cnt,cnt2);
			
		//	printMatrix(result,col,augCol);
		//	printf("\n");
			
			result = subtractReduced(result,cnt-cnt2,cnt,result[cnt-cnt2][cnt]);
			
		}
	}
	//printMatrix(result,col,augCol);
	//printf("\n");

	//printMatrix(result,row,augCol);
	return result;
}

// retrieve the matrix after GE
double ** retrieveMatrix(double **mx, int row){
	double ** result = (double**)malloc(row*sizeof(double*));

	int cnt,cnt2;

	for(cnt = 0; cnt < row; cnt++){
		result[cnt] = (double *)malloc(row * sizeof(double));
	}
	
	for(cnt = 0; cnt < row; cnt++){
		for(cnt2 = 0; cnt2 < augCol; cnt2++){
			result[cnt][cnt2] = mx[cnt][cnt2+row];
		}
	}
	//printMatrix(result,row,row);
	return result;
}


// finding the inverse of a matrix
double ** inverse(double **mx){
	double ** result = mx;
	////printf("\nREF\n");
	result = ref(result,col);
	////printMatrix(result,col,augCol);
	
	////printf("\nRREF\n");
	result = rref(result, col);
	
	
	////printMatrix(result,col,augCol);


	//printf("\nRetrieved Matrix\n");
	result = retrieveMatrix(result, col);
	//printMatrix(result,row,augCol);
	return result;
}


double * findWeight(double ** tp, double **mx, double *v){

	double ** square = matrixXMatrix(tp,mx,col,col,row);
	////printf("\nSquare Matrix\n");
	////printMatrix(square,col,col);

	square = augmentedMatrix(square,col);
	////printf("\nAugmented Matrix\n");
	////printMatrix(square,col,augCol);

	square = inverse(square);
	////printf("\nInverse Matrix\n");
	////printMatrix(square,col,col);

	square = matrixXMatrix(square,tp,col,row,col);
	//printf("\nInverse x Transpose\n");
	//printMatrix(square,col,row);

	//printf("\nWeight Vector\n");
	double * w = matrixXVector(square,v,col,row);
	//printVector(w,col);
	return w;
}

// free memory
void freeMem(){

}

int main(int argc, char*argv[]){
	////printf("*** TAKE IN TRAINING FILE AND CALCULATE ***\n");	

	FILE *train;
	
	train = fopen(argv[1],"r");
	
	if(argc < 2){
		printf("error\n");
		return 0;
	}

		if(train == NULL){
		printf("error\n");
		return 0;
	}

	fscanf(train,"%d",&col);
	////printf("%d\n",col);
	fscanf(train,"%d",&row);
	////printf("%d\n",row);
	col++;

	double **data = (double **)malloc(row * sizeof(double *));	
	double *price = (double *)malloc(row * sizeof(double));

	int cnt;
	for(cnt = 0; cnt < row; cnt++){
		data[cnt] = (double *)malloc(col * sizeof(double));
	}

	char seperator;
	int cnt2;
	for(cnt = 0; cnt < row; cnt++){
		for(cnt2 = 0; cnt2 < col; cnt2++){
			if(cnt2+1 == col){
				fscanf(train,"%lf%c",&data[cnt][cnt2],&seperator);
				fscanf(train,"%lf%c",&price[cnt],&seperator);
			}
			else if(cnt2 == 0)
				data[cnt][cnt2] = 1;
			else
				fscanf(train,"%lf%c",&data[cnt][cnt2],&seperator);
		}
	}

	
	double ** augMx = (double **)malloc(row * sizeof(double *));
	for(cnt = 0; cnt < row; cnt++){
		augMx[cnt] = (double *)malloc(augCol * sizeof(double));
	}


	////printf("Original Matrix\n");
	////printMatrix(data,row,col);

	//printf("\nPrice Vector\n");
	//printVector(price,row);
	

	////printf("\nTranpose Matrix\n");
	double ** tran = transpose(data);
	////printMatrix(tran,col,row);
	
	double * weight = findWeight(tran,data,price);	

	//printf("\nWeight\n");
	//printVector(weight,col);
	


	////printf("*** TAKE IN TESTING FILE ***\n");
	
	FILE *test;
	test = fopen(argv[2],"r");

	int testRow;
	fscanf(test,"%d",&testRow);
	////printf("Test row = %d\n", testRow);

	
	double **testData = (double **)malloc(testRow * sizeof(double *));	

	for(cnt = 0; cnt < testRow; cnt++){
		testData[cnt] = (double *)malloc(col * sizeof(double));
	}

	for(cnt = 0; cnt < testRow; cnt++){
		for(cnt2 = 0; cnt2 < col; cnt2++){
			if(cnt2 == 0)
				testData[cnt][cnt2] = 1;
			else
				fscanf(test,"%lf%c",&testData[cnt][cnt2],&seperator);
			//printf("testdata = %lf\n",testData[cnt][cnt2]);
		}
	}
	////printf("\n");
	////printMatrix(testData,testRow,col);


	

	// *** GETTING RESULTS ***
	double * prediction = matrixXVector(testData,weight,testRow,col);
	////double prediction[testRow];
	//printVector(prediction,testRow);


	////printf("\nResult\n");
	for(cnt = 0; cnt < testRow; cnt++){
		//printf("%lf\n",prediction[cnt]);
		printf("%0.0lf\n",prediction[cnt]);
	}



	fclose(train);
	fclose(test);
	return 0;
}
