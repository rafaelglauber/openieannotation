unit utypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  MSG_CAPTION_QUESTION       = 'Question';
  MSG_CAPTION_WARNING        = 'Warning';
  MSG_CAPTION_ERRO           = 'Error';
  MSG_MESSAGE_PENDENT_UPDATE = 'Do you want to save changes?';
  MSG_MESSAGE_UNIFY_FILES    = 'Do you want to unify the annotation files?';
  MSG_MESSAGE_NO_PENDINGS    = 'No change in current file.';
  MSG_MESSAGE_NO_VALUE       = 'The field needs a value.';
  MSG_MESSAGE_NO_ANNOTATIONF = 'There are no annotation in progress.';
  MSG_MESSAGE_NO_CURRENTSENT = 'A sentence for annotation has not been selected.';
  MSG_MESSAGE_NO_FACTS       = 'There are no facts to remove it.';
  MSG_MESSAGE_DUPLICATED     = 'This fact is duplicated, please modify!';
  MSG_MESSAGE_DUPLICATED_FILE = 'This file is duplcated!';
  MSG_MESSAGE_MIN_NUM_FILES  = 'Select at least 2 different annotation files.';
  MSG_MESSAGEM_INVALID_FILES = 'The annotation files are invalid. Please select files with the same amount of sentences.';
  MSG_MESSAGEM_CLOSE_FILE    = 'Close the in-progress annotation to calculate the agreement.';

const
  FILE_AGREEMENT_STEP1 = 'agree1.txt';
  FILE_RETURN_STEP1    = 'step1.txt';
  FILE_AGREEMENT_STEP2 = 'agree1.txt';
  FILE_RETURN_STEP2    = 'step1.txt';

implementation

end.

