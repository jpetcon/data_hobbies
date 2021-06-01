# -*- coding: utf-8 -*-
"""Facematch.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1njOu7yavuZdDuyYwrJzKoHael-LyaiQ2
"""

from google.colab import drive
drive.mount('/content/drive')

!apt-get -qq install -y libsm6 libxext6
!pip install -q -U opencv-python

from google.colab import files
file = files.upload()

"""Import Libraries"""

import urllib.request
import tarfile

import scipy.io
import numpy as np

from datetime import datetime, timedelta
import time

import tensorflow as tf

import keras
from keras.preprocessing import image
from keras.callbacks import ModelCheckpoint,EarlyStopping
from keras.layers import Dense, Activation, Dropout, Flatten, Input, Convolution2D, ZeroPadding2D, MaxPooling2D, Activation
from keras.layers import Conv2D, AveragePooling2D
from keras.models import Model, Sequential
from keras import metrics
from keras.models import model_from_json

from sklearn.model_selection import train_test_split
from sklearn.metrics.pairwise import cosine_similarity

import matplotlib.pyplot as plt

import cv2

import pandas as pd

"""Download and Extract Files"""

urllib.request.urlretrieve("https://data.vision.ee.ethz.ch/cvl/rrothe/imdb-wiki/static/imdb_crop.tar", "face_photos.tar")

urllib.request.urlretrieve("https://github.com/opencv/opencv/blob/master/data/haarcascades/haarcascade_frontalface_default.xml", "haarcascade_frontalface_default.xml")

tarfile.open("face_photos.tar").extractall()

"""Transform Dataset"""

photo_matrix = scipy.io.loadmat('imdb_crop/imdb.mat')
columns = ["dob", "photo_taken", "full_path", "gender", "name", "face_location", "face_score", "second_face_score", "celeb_names", "celeb_id"]

instances = photo_matrix['imdb'][0][0][0].shape[1]
df = pd.DataFrame(index = range(0,instances), columns = columns)


for i in photo_matrix:
    if i == "imdb":
        current_array = photo_matrix[i][0][0]
        for j in range(len(current_array)):
            df[columns[j]] = pd.DataFrame(current_array[j][0])

"""Remove No Faces, Unclear Faces and Multiple Faces"""

df = df.sample(frac=0.02)

# Only Clear Faces
df = df[df['face_score'] >= 3]

# Only Single Faces
df = df[df['second_face_score'].isna()]


df.count()

"""Convert Image to Pixels"""

## Convert Photos to Pixels for OpenCV

def ConvImagePixels(image_path):
    return cv2.imread("imdb_crop/" + image_path[0])
 
df['pixels'] = df['full_path'].apply(ConvImagePixels)

"""Load Face Recognition Model"""

#Ref: https://sefiks.com/2018/08/06/deep-face-recognition-with-keras/
 
def loadVggFaceModel():
    model = Sequential()
    model.add(ZeroPadding2D((1,1),input_shape=(224,224, 3)))
    model.add(Convolution2D(64, (3, 3), activation='relu'))
    model.add(ZeroPadding2D((1,1)))
    model.add(Convolution2D(64, (3, 3), activation='relu'))
    model.add(MaxPooling2D((2,2), strides=(2,2)))
 
    model.add(ZeroPadding2D((1,1)))
    model.add(Convolution2D(128, (3, 3), activation='relu'))
    model.add(ZeroPadding2D((1,1)))
    model.add(Convolution2D(128, (3, 3), activation='relu'))
    model.add(MaxPooling2D((2,2), strides=(2,2)))
 
    model.add(ZeroPadding2D((1,1)))
    model.add(Convolution2D(256, (3, 3), activation='relu'))
    model.add(ZeroPadding2D((1,1)))
    model.add(Convolution2D(256, (3, 3), activation='relu'))
    model.add(ZeroPadding2D((1,1)))
    model.add(Convolution2D(256, (3, 3), activation='relu'))
    model.add(MaxPooling2D((2,2), strides=(2,2)))
 
    model.add(ZeroPadding2D((1,1)))
    model.add(Convolution2D(512, (3, 3), activation='relu'))
    model.add(ZeroPadding2D((1,1)))
    model.add(Convolution2D(512, (3, 3), activation='relu'))
    model.add(ZeroPadding2D((1,1)))
    model.add(Convolution2D(512, (3, 3), activation='relu'))
    model.add(MaxPooling2D((2,2), strides=(2,2)))
 
    model.add(ZeroPadding2D((1,1)))
    model.add(Convolution2D(512, (3, 3), activation='relu'))
    model.add(ZeroPadding2D((1,1)))
    model.add(Convolution2D(512, (3, 3), activation='relu'))
    model.add(ZeroPadding2D((1,1)))
    model.add(Convolution2D(512, (3, 3), activation='relu'))
    model.add(MaxPooling2D((2,2), strides=(2,2)))
 
    model.add(Convolution2D(4096, (7, 7), activation='relu'))
    model.add(Dropout(0.5))
    model.add(Convolution2D(4096, (1, 1), activation='relu'))
    model.add(Dropout(0.5))
    model.add(Convolution2D(2622, (1, 1)))
    model.add(Flatten())
    model.add(Activation('softmax'))
 
    vgg_face_descriptor = Model(inputs=model.layers[0].input, outputs=model.layers[-2].output)
    return vgg_face_descriptor
 
model = loadVggFaceModel()
 
from keras.models import model_from_json
model.load_weights('/content/drive/My Drive/vgg_face_weights.h5')

"""Run Model to create image vectors"""

def findFaceRepresentation(detected_face):
    try:
        detected_face = cv2.resize(detected_face, (224, 224))
 
        #normalize detected face in scale of -1, +1
        img_pixels = image.img_to_array(detected_face)
        img_pixels = np.expand_dims(img_pixels, axis = 0)
        img_pixels /= 127.5
        img_pixels -= 1
 
        representation = model.predict(img_pixels)[0,:]
    except:
        representation = None
 
    return representation
 
df['face_vector_raw'] = df['pixels'].apply(findFaceRepresentation)

"""Transform Own Photo Into Vectors"""

img = cv2.imread("sophie.jpg")
plt.imshow(cv2.cvtColor(img, cv2.COLOR_BGR2RGB))
 
face_cascade = cv2.CascadeClassifier(cv2.data.haarcascades + 'haarcascade_frontalface_default.xml')
faces = face_cascade.detectMultiScale(img, 1.3, 5)
 
for (x,y,w,h) in faces:
    detected_face = img[int(y):int(y+h), int(x):int(x+w)]
 
    try: #add 10% margin around the face
        margin = 10
        margin_x = int((w * margin)/100); margin_y = int((h * margin)/100)
        detected_face = img[int(y-margin_y):int(y+h+margin_y), int(x-margin_x):int(x+w+margin_x)]
    except:
        print("detected face has no margin")
 
    detected_face = cv2.resize(detected_face, (224, 224))
 
#normalize in [-1, +1]
img_pixels = image.img_to_array(detected_face)
img_pixels = np.expand_dims(img_pixels, axis = 0)
img_pixels /= 127.5
img_pixels -= 1
 
test_photo = model.predict(img_pixels)[0,:]

def findCosineSimilarity(source_representation, test_representation=test_photo):
    try:
        a = np.matmul(np.transpose(source_representation), test_representation)
        b = np.sum(np.multiply(source_representation, source_representation))
        c = np.sum(np.multiply(test_representation, test_representation))
        return 1 - (a / (np.sqrt(b) * np.sqrt(c)))
    except:
        return 10 #assign a large value in exception. similar faces will have small value.
 
df['distance'] = df['face_vector_raw'].apply(findCosineSimilarity)

match_frame = df

match_frame = match_frame[match_frame['gender'] == 1.0]
match_frame = match_frame.sort_values(by=['distance'], ascending=True)

 
for i in range(0, 10):
    instance = match_frame.iloc[i]
    name = instance['name']
    distance = instance['distance']
    full_path = instance['full_path'][0]
    img = cv2.imread("imdb_crop/" + full_path)
    print(i,".",name," (",distance,")")
    plt.axis('off')
    plt.imshow(cv2.cvtColor(img, cv2.COLOR_BGR2RGB))
    plt.show()

df.to_csv('data.csv')
!cp data.csv "drive/My Drive/"