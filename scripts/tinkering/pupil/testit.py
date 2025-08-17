"""
To try this out using Python 3.10:
- download https://github.com/openPupil/PyPupilEXT/releases/download/v0.0.1-beta/PyPupilEXT-0.0.1-cp310-cp310-macosx_14_0_universal2.whl
- rename to PyPupilEXT-0.0.1-cp310-none-any.whl
- pip install -r requirements.txt
"""

from time import time
import json

import pypupilext as pp

import numpy as np
import cv2 as cv

import requests


HEADERS = {
    "Content-Type": "application/json"
}

def send_to_server(capture_time, outline_confidence, diameter):
    url = "http://localhost:5003/event" # FIXME
    data = {
        "eventType": "Pupil",
        "payload": json.dumps({
            "captureTime": capture_time,
            "confidence": outline_confidence,
            "diameter": diameter,
        })
    }

    response = requests.post(url, json=data, headers=HEADERS)

    if response.status_code != 202:
        print(f"Expected status code 202 but got {response.status_code} and ${response.text}")


def main():
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

            capture_time = time()

            print(capture_time, pupil.outline_confidence, pupil.diameter())

            send_to_server(capture_time, pupil.outline_confidence, pupil.diameter())

            # Display the resulting frame
            # cv.imshow('frame', gray)
            # if cv.waitKey(1) == ord('q'):
            #     break
    except KeyboardInterrupt:
        print("Done")

    # When everything done, release the capture
    cap.release()
    cv.destroyAllWindows()


if __name__ == "__main__":
    main()
