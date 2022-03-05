# Research Project : Revealing neural mechanisms of redundancy gain using a voxel-wise background connectivity analysis

## Index
  - [Authors](#authors) 
  - [About](#about) 
  - [Overview](#overview) 
  - [Abstract](#abstract)
  - [Files](#files)
  - [License](#license)

## Authors
- **Taehoon Kim**, Kyuin Kim, & Ghootae Kim
- Deep Memory Lab, Cognitive Science Research Group, Korea Brain Research Institute

## About
- 이 Repository는 행동 및 fMRI 인지 실험의 데이터 분석 작업물을 담고 있습니다.
- 실험 프로그램와 fMRI 영상 데이터 전처리 등을 위한 코드, 개별 참가자의 원본 데이터는 포함되어 있지 않습니다. 
- 분석은 R과 R studio를 통해 수행되었으며, 분석 코드 및 결과는 **fBGRED_results.html**, **fBGRED_results.md**를 다운로드받아서 확인할 수 있습니다. 
- 다른 파일에 대한 정보는 아래 **FILES**에서 확인할 수 있습니다.

## Overview
- 초기 시각 피질에서 입력이 동일함에도 활성화가 증가하는 중복 이득 (Redundancy Gain) 효과의 메커니즘을 연구.
- fMRI 복셀 기반 연결성 분석을 통해 상위 영역의 피드백이 중복 이득 효과를 이끈다는 것을 검증.

## Abstract

A previous study demonstrated that the retinotopic cortex shows greater activity when identical stimuli are simultaneously presented on different visual quadrants, than when different stimuli are presented, which is termed ‘redundancy gain’. A theoretical account for the phenomenon has been only suggestive: Responses in the early visual cortex are enhanced by feedback from higher cortical areas with larger receptive fields. Here we found the direct evidence supporting this account in an fMRI study. We calculated the voxel-wise functional connectivity between the early visual cortex and the higher-level scene-selective region (PPA) by using the background connectivity analysis. We then examined how the strength of this functional connectivity modulates the redundancy gain. In the first phase of an experiment, four identical (same condition) or four different scene images (different condition) were simultaneously presented in each visual quadrant. We measured the redundancy gain in the early visual cortex by comparing the same and different conditions. In the second phase, participants were asked to attend to scene images while ignoring superimposed face images. Using this task, we identified the voxel-wise functional connectivity between the early visual cortex and PPA independently of stimulus-evoked responses. In line with the previous theoretical account that feedback from higher visual areas drives the redundancy gain effect, the voxels in the early visual cortex (V1-V4) showed greater redundancy gain when they had higher functional connectivity with PPA. Our findings shed light on the interactive mechanism explaining neural responses in the early retinotopic cortex.

<br>

Keywords: redundancy gain, attention, voxel-wise background functional connectivity, fmri

## Files
- fBGRED_results.Rmd : 데이터 분석을 위한 RMarkDown source code
- fBGRED_results.html : 분석 코드 및 결과, 다운로드 후 확인 가능
- fBGRED_results.md : github용 분석 코드 및 결과. 내용은 fBGRED_results.html과 동일
- KSBNS2021.fbgRED.thk.pdf : 한국뇌신경과학회 2021 학술대회 포스터
- data : raw data 폴더, 비어있음


## License

```
MIT License

Copyright (c) 2021 Kim, Taehoon

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

```
