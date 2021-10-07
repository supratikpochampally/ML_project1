#include <chrono>
#include <fstream>
#include <vector>
#include <cmath>
#include <iostream>

using namespace std;

int count_survived(vector <int> v) {
    int count = 0; 
    for (int i = 0; i < v.size(); i++) {
        if (v.at(i) == 1) {
            count++;
        }
    }
    return count;
}

double[] apriori(vector <int> v) {
    int [2] p;
    int num_survived = count_survived(v);
    int num_dead = v.size() - num_survived;
    p[0] = num_dead / v.size();
    p[1] = num_survived / v.size();
}

double[][] lh_pclass (vector <int> ) {

}

int main() {
    ifstream inFS;                                  // Initialize input file stream object
    string line;
    string dummy_in, pclass_in, survived_in, sex_in, age_in;  // String to read in strings of values

    // Initialize vectors
    int pclass[1046];
    int survived[1046];
    int sex[1046];
    int age[1046];

    inFS.open("titanic_project.csv");
    if (!inFS.is_open()) {
        cout << "Could not open file titanic_project.csv" << endl;
        return 1;
    }

    // Read and print headings debug statement
    getline(inFS, line);
    int num = 0;
    while(inFS.good()) {
        // Read next line and parse by comma into strings
        getline(inFS, dummy_in, ',');
        getline(inFS, pclass_in, ',');
        getline(inFS, survived_in, ',');
        getline(inFS, sex_in, ',');
        getline(inFS, age_in, '\n');

        // Add int values to respective vectors
        pclass[num] = (stoi(pclass_in));
        survived[num] = (stoi(survived_in));
        sex[num] = (stoi(sex_in));
        age[num] = (stoi(age_in));

        // cout << pclass[num] << " " << survived[num] << " " << sex[num] << " " << age[num] << endl;
        num++;
    }

    // Initialize training set
    int pclass_train[900];
    int survived_train[900];

    // Initialize testing set
    int pclass_test[146];
    int survived_test[146];

    // Split into training and testing sets
    for (int i = 0; i < 900; i++) {
        pclass_train[i] = pclass[i];
        survived_train[i] = survived[i];
    }

    for (int i = 900; i < 1046; i++) {
        pclass_test[i - 900] = pclass[i];
        survived_test[i - 900] = survived[i];
    }

    // Set up code for Logistic Regression (sigmoid() function above main() function)
    int data_matrix [900][2];
    int data_matrix_transpose [2][900];
    double labels [900][1];

    for (int i = 0; i < 900; i++) {
        data_matrix[i][0] = 1;
        data_matrix[i][1] = pclass_train[i];
        data_matrix_transpose[0][i] = 1;
        data_matrix_transpose[1][i] = pclass_train[i];
        labels[i][0] = survived_train[i];
    }

    double weights[2][1] = {1, 1};
    double error [900][1];
    double learning_rate = 0.001;
    double prob_vector [900][1];

    auto start = chrono::high_resolution_clock::now();

    for (int i = 0; i < 500000; i++) {
        double x = 0;
        double y = 0;
        for (int j = 0; j < 900; j++) {
            prob_vector[j][0] = sigmoid((data_matrix[j][0] * weights[0][0]) + (data_matrix[j][1] * weights[1][0]));
            error[j][0] = labels[j][0] - prob_vector[j][0];
        }   
        for (int j = 0; j < 900; j++) {
            x += data_matrix_transpose[0][j] * error[j][0];
            y += data_matrix_transpose[1][j] * error[j][0];
        }
        weights[0][0] += learning_rate * x / 900;
        weights[1][0] += learning_rate * y / 900;
    }

    auto stop = chrono::high_resolution_clock::now();
    chrono::duration<double> elapsed_sec = stop - start;

    cout << "Weights: " << weights[0][0] << " " << weights[1][0] << endl;

    //test
    double TP = 0;
    double FP = 0;
    double FN = 0;
    double TN = 0;

    double intercept = weights[0][0];
    double slope = weights[1][0];

    for (int i = 0; i < 146; i++) {
        double probs = sigmoid(slope * pclass_test[i] + intercept);

        int pred = (probs > 0.5) ? 1 : 0;
        int real = survived_test[i];

        if (pred == 1 && real == 1) {
            TN++;
        } else if (pred == 1 && real == 0) {
            FN++;
        } else if (pred == 0 && real == 1) {
            FP++;
        } else {
            TP++;
        }
    }

    cout << TP << " " << FP << endl;
    cout << FN << " " << TN << endl;

    double accuracy = (TP + TN) / (TP + FP + FN + TN);
    double sensitivity = TP / (TP + FN);
    double specificity = TN / (TN + FP);

    cout << "Accuracy = " << accuracy << endl;
    cout << "Sensitivity = " << sensitivity << endl;
    cout << "Specificity = " << specificity << endl;

    cout << "Time: " << elapsed_sec.count() << endl;
}