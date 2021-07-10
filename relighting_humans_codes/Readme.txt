+-----------------------------------------------------------
| Relighting Humans: Occlusion-Aware Inverse Rendering for Full-Body Human Images
| Project page: http://kanamori.cs.tsukuba.ac.jp/projects/relighting_human/
| Readme
+-----------------------------------------------------------

This directory contains source codes of the following paper:

> Yoshihiro Kanamori, Yuki Endo: "Relighting Humans: Occlusion-Aware Inverse Rendering
> for Full-Body Human Images," ACM Transactions on Graphics (Proc. of SIGGRAPH Asia 2018), 
> 37, 6, Article No. 270, November 2018.

[Dependencies]
- Python 2.7
- Chainer (we used version 1.24.0): http://chainer.org
- Python module of OpenCV (we used version 3.3.0 with OpenEXR support): https://opencv.org/
- ffmpeg (for the relighting script): https://www.ffmpeg.org/

We tested our codes only on Ubuntu 16.04.2 LTS with NVIDIA GeForce 1080 Ti, but they should work on other platforms (with slight modifications).

[How to use]
Note: each script shows its usage with "--help" option.

$ python train.py
Training with test outputs. Sample image data are also included. Note that light data for training and test are required, which can be downloaded from the project page. The resolution of input images must be 1024x1024.

$ python test_with_photos.py
Inference with photos plus masks. Note that a trained model is required, which can be downloaded from the project page. Each photo is trimmed and resized to 1024x1024 before processed, and is ersized back to the original size.

$ python relight.py
Relighting with a specified light. Note that inferred data (typically outputs by test_with_photos.py) must be already prepared. "ffmpeg" is also required for outputting mp4 movies.

You can use the codes for scientific purposes only. Use in commercial projects and redistribution are not allowed without author's permission. Please cite our paper when using the codes. 

=============================
Contact Information
=============================
Yoshihiro Kanamori kanamori@cs.tsukuba.ac.jp
Yuki Endo endo@val.cs.tut.ac.jp