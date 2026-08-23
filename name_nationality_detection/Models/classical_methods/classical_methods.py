import pandas as pd
import numpy as np
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import classification_report, accuracy_score
from imblearn.over_sampling import SMOTE
from xgboost import XGBClassifier
from sklearn.naive_bayes import MultinomialNB
from sklearn.linear_model import LogisticRegression

class NameNationalityClassifier:
    def __init__(self, model_type='xgboost', test_size=0.2, random_state=42):
        self.model_type = model_type
        self.test_size = test_size
        self.random_state = random_state
        self.vectorizer = CountVectorizer(analyzer='char', ngram_range=(1, 3))
        self.label_encoder = LabelEncoder()
        self.model = None

    def preprocess_data(self, data):
        """
        Preprocess the data by vectorizing names, encoding labels, and handling imbalance.
        :param data: DataFrame with 'name' and 'nationality' columns.
        :return: X_train, X_test, y_train, y_test
        """
        # Vectorize names and encode labels
        X = self.vectorizer.fit_transform(data['name'])
        y = self.label_encoder.fit_transform(data['nationality'])

        # Split data into training and testing sets
        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=self.test_size, random_state=self.random_state)

        # Handle class imbalance using SMOTE
        smote = SMOTE(random_state=self.random_state)
        X_train, y_train = smote.fit_resample(X_train, y_train)

        return X_train, X_test, y_train, y_test

    def initialize_model(self):
        """
        Initialize the model based on the selected type.
        """
        if self.model_type == 'xgboost':
            self.model = XGBClassifier(use_label_encoder=False, eval_metric='mlogloss', scale_pos_weight=1)
        elif self.model_type == 'naive_bayes':
            self.model = MultinomialNB()
        elif self.model_type == 'logistic_regression':
            self.model = LogisticRegression(max_iter=1000, solver='lbfgs', class_weight='balanced')
        else:
            raise ValueError("Invalid model_type. Choose from 'xgboost', 'naive_bayes', 'logistic_regression'.")

    def train(self, X_train, y_train):
        """
        Train the model on the training data.
        :param X_train: Feature matrix for training.
        :param y_train: Labels for training.
        """
        self.initialize_model()
        self.model.fit(X_train, y_train)

    def evaluate(self, X_test, y_test):
        """
        Evaluate the model on the test data.
        :param X_test: Feature matrix for testing.
        :param y_test: Labels for testing.
        """
        y_pred = self.model.predict(X_test)
        print("Accuracy:", accuracy_score(y_test, y_pred))
        print("Classification Report:\n", classification_report(y_test, y_pred, target_names=self.label_encoder.classes_))

    def predict(self, names):
        """
        Predict the nationality of the given names.
        :param names: List of names to classify.
        :return: List of predicted nationalities.
        """
        X = self.vectorizer.transform(names)
        y_pred = self.model.predict(X)
        return self.label_encoder.inverse_transform(y_pred)

if __name__ == '__main__':
    # Example dataset (replace with your data source)
    data = pd.DataFrame({
        'name': ['John Smith', 'Marie Curie', 'Li Wei', 'Ahmed Ali', 'Carlos Garcia'],
        'nationality': ['American', 'French', 'Chinese', 'Egyptian', 'Spanish']
    })

    # Initialize classifier (change model_type as needed)
    classifier = NameNationalityClassifier(model_type='xgboost')

    # Preprocess data
    X_train, X_test, y_train, y_test = classifier.preprocess_data(data)

    # Train and evaluate
    classifier.train(X_train, y_train)
    classifier.evaluate(X_test, y_test)

    # Predict new names
    new_names = ['Anna Ivanova', 'Hiroshi Tanaka']
    predictions = classifier.predict(new_names)
    print("Predictions:", predictions)
