"""
To try this out using Python 3.10:
- download https://github.com/openPupil/PyPupilEXT/releases/download/v0.0.1-beta/PyPupilEXT-0.0.1-cp310-cp310-macosx_14_0_universal2.whl
- rename to PyPupilEXT-0.0.1-cp310-none-any.whl
- pip install -r requirements.txt
"""

import pypupilext as pp

import numpy as np
import cv2 as cv

algorithm = pp.PuRe()

cap = cv.VideoCapture(0)
if not cap.isOpened():
    print("Cannot open camera")
    exit()

try:
    while True:
        # Capture frame-by-frame
        ret, frame = cap.read()
     
        # if frame is read correctly ret is True
        if not ret:
            print("Can't receive frame (stream end?). Exiting ...")
            break
        # Our operations on the frame come here
        gray = cv.cvtColor(frame, cv.COLOR_BGR2GRAY)

        pupil = algorithm.runWithConfidence(gray)

        print(pupil.diameter())
        print(pupil.outline_confidence)

        # Display the resulting frame
        # cv.imshow('frame', gray)
        # if cv.waitKey(1) == ord('q'):
        #     break
except KeyboardInterrupt:
    pass
 
# When everything done, release the capture
cap.release()
cv.destroyAllWindows()
