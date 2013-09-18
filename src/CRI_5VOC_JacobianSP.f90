! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Sparse Jacobian Data Structures File
! 
! Generated by KPP-2.2.3 symbolic chemistry Kinetics PreProcessor
!       (http://www.cs.vt.edu/~asandu/Software/KPP)
! KPP is distributed under GPL, the general public licence
!       (http://www.gnu.org/copyleft/gpl.html)
! (C) 1995-1997, V. Damian & A. Sandu, CGRER, Univ. Iowa
! (C) 1997-2005, A. Sandu, Michigan Tech, Virginia Tech
!     With important contributions from:
!        M. Damian, Villanova University, USA
!        R. Sander, Max-Planck Institute for Chemistry, Mainz, Germany
! 
! File                 : CRI_5VOC_JacobianSP.f90
! Time                 : Tue Aug 27 15:48:09 2013
! Working directory    : /home/hes134/kpp-2.2.3/CRI_5VOC
! Equation file        : CRI_5VOC.kpp
! Output root filename : CRI_5VOC
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE CRI_5VOC_JacobianSP

  PUBLIC
  SAVE


! Sparse Jacobian Data


  INTEGER, PARAMETER, DIMENSION(360) :: LU_IROW_0 = (/ &
       1,  1,  1,  2,  2,  3,  3,  3,  3,  4,  4,  4, &
       5,  5,  6,  6,  7,  7,  8,  8,  8,  9,  9, 10, &
      10, 10, 11, 11, 11, 12, 12, 12, 13, 13, 13, 13, &
      14, 14, 14, 15, 15, 15, 16, 16, 16, 16, 16, 17, &
      17, 17, 17, 18, 18, 18, 18, 18, 19, 19, 19, 20, &
      20, 20, 21, 21, 21, 21, 22, 22, 22, 22, 22, 23, &
      23, 23, 24, 24, 24, 24, 25, 25, 25, 25, 26, 26, &
      26, 26, 27, 27, 27, 27, 28, 28, 28, 28, 29, 29, &
      29, 29, 30, 30, 30, 30, 31, 31, 31, 31, 32, 32, &
      32, 32, 33, 33, 33, 33, 34, 34, 34, 34, 35, 35, &
      35, 35, 36, 36, 36, 36, 37, 37, 37, 37, 38, 38, &
      38, 38, 39, 39, 39, 39, 39, 39, 40, 40, 40, 40, &
      41, 41, 41, 41, 42, 42, 42, 42, 43, 43, 43, 43, &
      44, 44, 44, 44, 44, 45, 45, 45, 45, 46, 46, 46, &
      46, 47, 47, 47, 47, 48, 48, 48, 48, 49, 49, 49, &
      49, 50, 50, 50, 50, 51, 51, 51, 51, 52, 52, 52, &
      52, 53, 53, 53, 53, 54, 54, 54, 54, 55, 55, 55, &
      55, 56, 56, 56, 56, 57, 57, 57, 57, 58, 58, 58, &
      58, 59, 59, 59, 59, 60, 60, 60, 60, 60, 61, 61, &
      61, 61, 61, 62, 62, 62, 62, 63, 63, 63, 63, 64, &
      64, 64, 64, 65, 65, 65, 65, 66, 66, 66, 66, 67, &
      67, 67, 67, 68, 68, 68, 68, 68, 69, 69, 69, 69, &
      70, 70, 70, 70, 71, 71, 71, 71, 72, 72, 72, 72, &
      73, 73, 73, 73, 73, 74, 74, 74, 74, 74, 74, 74, &
      75, 75, 75, 75, 76, 76, 76, 76, 76, 76, 76, 76, &
      76, 76, 76, 77, 77, 77, 77, 77, 77, 77, 78, 78, &
      78, 78, 78, 79, 79, 79, 79, 79, 79, 79, 79, 80, &
      80, 80, 80, 80, 80, 80, 80, 80, 81, 81, 81, 81, &
      82, 82, 82, 82, 83, 83, 83, 83, 83, 83, 83, 83, &
      84, 84, 84, 84, 84, 84, 84, 85, 85, 85, 85, 85 /)
  INTEGER, PARAMETER, DIMENSION(360) :: LU_IROW_1 = (/ &
      85, 85, 85, 85, 85, 85, 85, 85, 85, 86, 86, 86, &
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 87, &
      87, 87, 87, 87, 87, 87, 87, 87, 88, 88, 88, 88, &
      88, 88, 88, 88, 89, 89, 89, 89, 89, 89, 89, 90, &
      90, 90, 90, 90, 90, 91, 91, 91, 91, 91, 91, 91, &
      91, 91, 91, 91, 92, 92, 92, 92, 92, 92, 92, 93, &
      93, 93, 93, 93, 93, 93, 93, 94, 94, 94, 94, 94, &
      94, 94, 94, 94, 94, 94, 94, 95, 95, 95, 95, 95, &
      95, 95, 95, 95, 96, 96, 96, 96, 96, 96, 96, 96, &
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, &
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, &
      97, 97, 97, 97, 97, 97, 98, 98, 98, 98, 98, 98, &
      98, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, &
      99, 99, 99, 99, 99, 99, 99, 99, 99,100,100,100, &
     100,100,100,100,101,101,101,101,101,101,101,101, &
     101,102,102,102,102,102,102,102,102,102,102,102, &
     103,103,103,103,103,103,103,103,103,103,103,103, &
     103,103,104,104,104,104,104,104,104,105,105,105, &
     105,105,105,105,106,106,106,106,106,106,106,107, &
     107,107,107,107,107,107,108,108,108,108,108,108, &
     108,108,108,108,108,108,108,108,108,108,108,109, &
     109,109,109,109,109,109,109,109,109,109,109,109, &
     109,109,110,110,110,110,110,110,110,110,110,110, &
     110,110,110,110,110,111,111,111,111,111,111,111, &
     111,111,112,112,112,112,112,112,112,112,112,112, &
     113,113,113,113,113,113,113,113,113,113,113,114, &
     114,114,114,114,114,114,114,114,114,114,114,115, &
     115,115,115,115,115,116,116,116,116,116,116,116, &
     116,117,117,117,117,117,117,117,117,117,117,117, &
     117,117,117,117,117,117,117,117,117,117,117,117 /)
  INTEGER, PARAMETER, DIMENSION(360) :: LU_IROW_2 = (/ &
     117,117,117,117,117,117,117,117,117,117,117,117, &
     117,117,117,118,118,118,118,118,118,118,118,118, &
     118,118,118,118,118,118,118,118,118,119,119,119, &
     119,119,119,119,119,119,119,119,119,119,119,119, &
     119,119,119,120,120,120,120,120,120,120,120,120, &
     120,120,120,120,120,120,120,120,120,120,120,120, &
     120,120,120,120,120,120,120,120,120,120,120,120, &
     120,120,120,120,120,120,120,121,121,121,121,121, &
     121,121,122,122,122,122,122,122,122,123,123,123, &
     123,123,123,123,123,123,123,123,123,124,124,124, &
     124,124,124,124,124,124,124,124,124,124,125,125, &
     125,125,125,125,125,125,125,125,125,125,125,125, &
     125,125,125,125,125,125,125,125,125,126,126,126, &
     126,126,126,126,126,126,126,126,126,126,126,126, &
     126,126,126,126,126,126,126,126,126,126,126,126, &
     126,126,126,126,126,126,126,126,126,126,126,126, &
     126,126,126,126,126,126,126,126,126,126,126,126, &
     126,126,126,126,126,126,126,126,126,126,126,126, &
     126,126,126,126,126,126,126,126,126,126,126,126, &
     126,126,126,126,126,126,126,126,126,126,126,126, &
     126,126,126,126,126,126,126,126,126,126,126,126, &
     126,126,126,126,126,126,126,126,126,126,126,126, &
     126,126,126,126,126,126,126,126,126,126,126,126, &
     126,127,127,127,127,127,127,127,127,127,127,127, &
     127,127,127,127,127,127,127,127,127,127,127,127, &
     127,127,127,127,127,127,127,127,127,127,127,127, &
     127,127,127,128,128,128,128,128,128,128,128,128, &
     128,128,128,128,128,128,128,128,128,128,128,128, &
     128,128,128,128,128,128,128,128,128,128,128,128, &
     128,128,128,128,128,128,128,128,128,128,128,128 /)
  INTEGER, PARAMETER, DIMENSION(190) :: LU_IROW_3 = (/ &
     128,128,128,128,128,128,128,128,128,128,128,128, &
     128,128,128,128,128,128,128,128,128,128,128,128, &
     128,128,128,128,128,128,128,128,128,128,128,128, &
     128,128,128,128,128,128,129,129,129,129,129,129, &
     129,129,129,129,129,129,129,129,129,129,129,129, &
     129,129,129,129,129,129,129,129,129,129,129,129, &
     129,129,129,129,129,129,129,129,129,129,129,129, &
     129,129,129,129,129,129,129,129,129,129,129,129, &
     129,129,129,129,129,129,129,129,129,129,129,129, &
     129,129,129,130,130,130,130,130,130,130,130,130, &
     130,130,130,130,130,130,130,130,130,130,130,130, &
     130,130,130,130,130,130,130,130,130,130,131,131, &
     131,131,131,131,131,131,131,131,131,131,131,131, &
     131,131,131,131,131,131,131,131,131,131,131,131, &
     131,131,131,131,131,131,131,131,131,131,131,131, &
     131,131,131,131,131,131,131,131,131,131 /)
  INTEGER, PARAMETER, DIMENSION(1270) :: LU_IROW = (/&
    LU_IROW_0, LU_IROW_1, LU_IROW_2, LU_IROW_3 /)

  INTEGER, PARAMETER, DIMENSION(360) :: LU_ICOL_0 = (/ &
       1,  8, 85,  2,  3,  3,  4, 14, 88,  4, 14,126, &
       5,113,  6,126,  7,126,  8,129,131,  9,126, 10, &
     117,126, 11,126,127, 12,109,126, 13, 80, 83,126, &
      14, 88,126, 15,125,126, 16, 81, 82,113,126,  7, &
      17,126,131, 17, 18,126,129,131, 19,102,126, 20, &
      71,126,  9, 21,126,131, 21, 22,126,129,131, 23, &
     106,126, 24,100,126,128, 25,124,126,128, 26,118, &
     126,128, 27, 89,126,128, 28,103,126,127, 29, 93, &
     126,127, 30,102,126,128, 31,106,126,128, 32,126, &
     128,129, 33,125,126,128, 34, 53, 77,126, 35, 92, &
     126,127, 36,119,126,127, 37,122,126,128, 38,120, &
     126,128, 39,101,113,114,126,128, 40,116,126,128, &
      41,103,126,128, 42, 90,126,128, 43,106,126,127, &
      44,105,107,126,127, 45,112,126,129, 46, 93,126, &
     128, 47,102,126,127, 48,115,126,128, 49,122,126, &
     127, 50, 95,126,128, 51, 97,126,128, 52,111,126, &
     128, 53,104,126,128, 54,109,126,127, 55,109,126, &
     128, 56, 98,126,128, 57,125,126,127, 58,120,126, &
     129, 59,112,126,128, 22, 60,126,129,131, 18, 61, &
     126,129,131, 62, 92,126,128, 63, 95,126,129, 64, &
      90,126,127, 65,123,126,128, 66,124,126,129, 67, &
     119,126,128, 61, 68,126,129,131, 69,115,126,127, &
      70, 97,126,127, 71,121,126,128, 72,111,126,129, &
      73,115,126,127,131, 37, 49, 74,122,126,127,128, &
      75,123,126,127, 42, 51, 64, 70, 76, 90, 97,126, &
     127,128,131, 29, 46, 77, 93,126,127,128, 78,105, &
     107,126,128, 65, 75, 79,123,126,127,128,131, 42, &
      64, 80, 90, 97,126,127,128,131, 81,113,126,131, &
      82,113,126,131, 51, 70, 83, 97,126,127,128,131, &
      24, 84,100,126,127,128,131, 17, 21, 60, 61, 85 /)
  INTEGER, PARAMETER, DIMENSION(360) :: LU_ICOL_1 = (/ &
     101,108,110,114,117,126,129,130,131, 23, 31, 40, &
      43, 48, 69, 86,106,115,116,126,127,128,131, 56, &
      84, 87, 98,100,126,127,128,131,  5, 14, 88,113, &
     126,127,129,131, 81, 89,113,126,127,128,131,  9, &
      90,126,127,128,131, 44, 60, 78, 91,105,107,126, &
     127,128,129,131, 82, 92,113,126,127,128,131, 20, &
      71, 93,121,126,127,128,131, 36, 67, 72, 94,111, &
     112,119,126,127,128,129,131, 50, 63, 95,110,126, &
     127,128,129,131, 56, 58, 63, 66, 72, 76, 81, 82, &
      84, 90, 95, 96, 97, 98, 99,100,101,108,110,111, &
     112,113,114,117,120,124,126,127,128,129,130,131, &
       7, 97,126,127,128,131, 84, 98,100,126,127,128, &
     131, 26, 59, 87, 94, 97, 98, 99,100,101,111,112, &
     113,114,118,119,126,127,128,129,131, 82,100,113, &
     126,127,128,131, 35, 62, 92,101,113,126,127,128, &
     131, 53, 74, 83, 97,102,104,122,126,127,128,131, &
      13, 15, 80, 81, 83, 90, 97,103,113,125,126,127, &
     128,131, 79,104,123,126,127,128,131, 37,105,122, &
     126,127,128,131, 73,106,115,126,127,128,131, 49, &
     107,122,126,127,128,131, 19, 30, 47, 68, 78,102, &
     104,105,107,108,122,123,126,127,128,129,131, 38, &
      55, 83, 86, 97,106,109,115,116,120,126,127,128, &
     130,131, 28, 41, 52, 59,101,103,110,111,112,113, &
     125,126,127,128,131, 52, 72,111,114,126,127,128, &
     129,131, 45, 59,101,112,113,126,127,128,129,131, &
      81, 82, 88,101,113,114,126,127,128,129,131, 45, &
      62, 82, 92,112,113,114,126,127,128,129,131,  6, &
     115,126,127,128,131, 40, 73,115,116,126,127,128, &
     131, 12, 27, 41, 50, 54, 55, 58, 62, 63, 67, 81, &
      87, 89, 92, 94, 95, 98,100,103,109,110,111,112 /)
  INTEGER, PARAMETER, DIMENSION(360) :: LU_ICOL_2 = (/ &
     113,114,115,116,117,118,119,120,125,126,127,128, &
     129,130,131, 20, 68, 71, 86, 91,105,106,107,115, &
     116,118,121,122,126,127,128,129,131, 19, 23, 34, &
      53, 77, 93,102,104,106,115,119,121,122,123,126, &
     127,128,131, 20, 34, 38, 40, 52, 53, 58, 71, 73, &
      74, 77, 79, 86, 87, 93, 94, 98, 99,100,101,104, &
     106,111,112,113,114,115,116,118,119,120,121,122, &
     123,126,127,128,129,130,131, 74,121,122,126,127, &
     128,131, 68,122,126,127,128,129,131, 71, 91,105, &
     107,121,122,123,126,127,128,129,131, 25, 66,108, &
     116,121,122,123,124,126,127,128,129,131, 25, 26, &
      48, 65, 69, 75, 79, 80, 90, 97,108,115,118,121, &
     122,123,124,125,126,127,128,129,131,  5,  6,  7, &
       9, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, &
      23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, &
      35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, &
      47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, &
      59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, &
      71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, &
      83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, &
      95, 96, 97, 98, 99,100,101,102,103,104,105,106, &
     107,108,109,110,111,112,113,114,115,116,117,118, &
     119,120,121,122,123,124,125,126,127,128,129,130, &
     131, 11, 88, 89, 90, 92, 93, 95, 97, 98,100,102, &
     103,104,105,106,107,109,110,111,112,113,114,115, &
     116,118,119,120,121,122,123,124,125,126,127,128, &
     129,130,131,  4,  7,  9, 10, 12, 14, 15, 16, 19, &
      23, 24, 30, 31, 32, 33, 39, 41, 42, 43, 46, 47, &
      50, 51, 54, 55, 56, 57, 59, 62, 64, 65, 67, 68, &
      70, 75, 76, 77, 78, 80, 81, 82, 83, 84, 88, 89 /)
  INTEGER, PARAMETER, DIMENSION(190) :: LU_ICOL_3 = (/ &
      90, 91, 92, 93, 94, 95, 96, 97, 98, 99,100,101, &
     102,103,104,105,106,107,108,109,110,111,112,113, &
     114,115,116,117,118,119,120,121,122,123,124,125, &
     126,127,128,129,130,131,  8, 11, 18, 22, 27, 28, &
      29, 32, 35, 36, 43, 44, 45, 47, 49, 54, 57, 58, &
      60, 61, 63, 64, 66, 69, 70, 72, 75, 85, 87, 88, &
      89, 90, 92, 93, 95, 97, 98,100,101,102,103,104, &
     105,106,107,108,109,110,111,112,113,114,115,116, &
     117,118,119,120,121,122,123,124,125,126,127,128, &
     129,130,131, 15, 33, 46, 48, 57, 65, 66, 67, 69, &
      75, 77, 78, 91, 93,104,105,107,115,116,119,121, &
     122,123,124,125,126,127,128,129,130,131,  8, 17, &
      21, 60, 61, 81, 82, 85, 88, 89, 90, 92, 93, 95, &
      97, 98,100,101,102,103,104,105,106,107,108,109, &
     110,111,112,113,114,115,116,117,118,119,120,121, &
     122,123,124,125,126,127,128,129,130,131 /)
  INTEGER, PARAMETER, DIMENSION(1270) :: LU_ICOL = (/&
    LU_ICOL_0, LU_ICOL_1, LU_ICOL_2, LU_ICOL_3 /)

  INTEGER, PARAMETER, DIMENSION(132) :: LU_CROW = (/ &
       1,  4,  6, 10, 13, 15, 17, 19, 22, 24, 27, 30, &
      33, 37, 40, 43, 48, 52, 57, 60, 63, 67, 72, 75, &
      79, 83, 87, 91, 95, 99,103,107,111,115,119,123, &
     127,131,135,141,145,149,153,157,162,166,170,174, &
     178,182,186,190,194,198,202,206,210,214,218,222, &
     227,232,236,240,244,248,252,256,261,265,269,273, &
     277,282,289,293,304,311,316,324,333,337,341,349, &
     356,370,384,393,401,408,414,425,432,440,452,461, &
     493,499,506,526,533,542,553,567,574,581,588,595, &
     612,627,642,651,661,672,684,690,698,736,754,772, &
     812,819,826,838,851,874,998,1036,1123,1192,1223,1271 /)

  INTEGER, PARAMETER, DIMENSION(132) :: LU_DIAG = (/ &
       1,  4,  6, 10, 13, 15, 17, 19, 22, 24, 27, 30, &
      33, 37, 40, 43, 49, 53, 57, 60, 64, 68, 72, 75, &
      79, 83, 87, 91, 95, 99,103,107,111,115,119,123, &
     127,131,135,141,145,149,153,157,162,166,170,174, &
     178,182,186,190,194,198,202,206,210,214,218,223, &
     228,232,236,240,244,248,252,257,261,265,269,273, &
     277,284,289,297,306,311,318,326,333,337,343,350, &
     360,376,386,395,402,409,417,426,434,443,454,472, &
     494,500,512,527,536,546,560,568,575,582,589,604, &
     618,633,644,654,665,678,685,693,725,746,764,802, &
     813,820,832,845,868,992,1031,1119,1189,1221,1270,1271 /)


END MODULE CRI_5VOC_JacobianSP
