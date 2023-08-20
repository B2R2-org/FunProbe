#!/root/anaconda3/envs/xda/bin/python

from fairseq.models.roberta import RobertaModel
import os
from collections import defaultdict
from colorama import Fore, Back, Style
import torch
import sys
from elftools.elf.elffile import ELFFile

bin_path = sys.argv[1]
res_path = sys.argv[2]
dev_id = int(sys.argv[3])

elf = ELFFile(open(bin_path, 'rb'))
text = elf.get_section_by_name('.text')

roberta = RobertaModel.from_pretrained('/packages/XDA/checkpoints/funcbound', 'checkpoint_best.pt',
                                       '/packages/XDA/data-bin/funcbound', bpe=None, user_dir='finetune_tasks')
roberta.cuda(dev_id)
roberta.eval()

text_data = text.data()
text_size = len(text_data)
text_addr = text['sh_addr']

funcs = set()

i = 0
while i * 512 < len(text_data):
  tokens = text_data[512*i:512*i+512]
  tokens = list(map(lambda x: hex(x)[2:].zfill(2).lower(), tokens))
  if len(tokens) < 512:
    tokens = tokens + ['00'] * (512 - len(tokens))

  encoded_tokens = roberta.encode(' '.join(tokens))
  logprobs = roberta.predict('funcbound', encoded_tokens)
  labels = logprobs.argmax(dim=2).view(-1).data

  for j, label in enumerate(labels):
    if label == 1:
      funcs.add(text_addr + 512*i + j)

  i += 1

with open(res_path, 'w') as f:
  for func in funcs:
    f.write('%x\n' % func)
