from __future__ import print_function
import pickle
import os.path
from googleapiclient.discovery import build
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request
import base64
from bs4 import BeautifulSoup
from googletrans import Translator
from email.mime.text import MIMEText

# If modifying these scopes, delete the file token.pickle.
SCOPES = ['https://www.googleapis.com/auth/gmail.modify']

def getMessage(msg):
    # Get value of 'payload' from dictionary 'msg' 
    payload = msg['payload']

    # Look for Sender Email in the headers 
    for d in payload['headers']: 
        if d['name'] == 'From': 
            sender = d['value'] 

    # The Body of the message is in Encrypted format. So, we have to decode it. 
    # Get the data and decode it with base 64 decoder. 
    data = payload['body']['data'] 
    data = data.replace("-","+").replace("_","/") 
    decoded_data = base64.b64decode(data) 

    # Now, the data obtained is in lxml. So, we will parse  
    # it with BeautifulSoup library 
    soup = BeautifulSoup(decoded_data , "lxml") 
    text = '\n'.join(soup.findAll(text = True)[0:-3])

    # Returning the sender's email and message 
    return (sender, text)

def langDetection(msg_text):
    translator = Translator()
    input_text = translator.detect(msg_text)
    message = translator.translate("Hello world", dest=input_text.lang)
    return message.text

def create_message(sender, to, subject, msg_text):
	try:
		message = MIMEText(msg_text)
		message['to'] = to
		message['from'] = sender
		message['subject'] = subject

		return {'raw': base64.urlsafe_b64encode(message.as_bytes()).decode()}
	except Exception as e:
		print('An error occurred: %s' % e)
		return 0

def send_message(service, user_id, msg):
    try:
        _ = service.users().messages().send(userId=user_id, body=msg).execute()
        print('Letter was sended')
    except Exception as e:
        print('An error occurred: %s' % e)
        print('Letter was not sended')

def main():
    """Shows basic usage of the Gmail API.
    Lists the user's Gmail labels.
    """
    creds = None
    # The file token.pickle stores the user's access and refresh tokens, and is
    # created automatically when the authorization flow completes for the first
    # time.
    if os.path.exists('token.pickle'):
        with open('token.pickle', 'rb') as token:
            creds = pickle.load(token)
    # If there are no (valid) credentials available, let the user log in.
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file(
                'credentials.json', SCOPES)
            creds = flow.run_local_server(port=0)
        # Save the credentials for the next run
        with open('token.pickle', 'wb') as token:
            pickle.dump(creds, token)

    service = build('gmail', 'v1', credentials=creds)

    # Call the Gmail API
    results = service.users().messages().list(userId='me', labelIds = ['Label_6842703095677337745']).execute()
    messages = results.get('messages')

    if not messages:
        print('No messages found.')
    else:
        print('Messages:')
        for message in messages:
            msg = service.users().messages().get(userId='me', id=message['id']).execute()
            try: 
                sender, msg_text = getMessage(msg)                
                print(sender)
                print(msg_text)

                output_msg = langDetection(msg_text)
                to = sender[sender.find('<'):].replace('<', '').replace('>', '')
                print(output_msg)
                print(to)

                msgToSend = create_message('me', to, 'Test', output_msg)
                send_message(service=service, user_id='me', msg=msgToSend)

                _ = service.users().messages().modify(userId='me', id=message['id'], body={'removeLabelIds': ['Label_6842703095677337745']})
            except Exception as e:
                print('An error occurred: %s' % e)

if __name__ == '__main__':
    main()