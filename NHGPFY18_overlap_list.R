overlap_well_gc <- subset(NHGPFY18_clean3, NHGPFY18_clean3$NHGP.NUP.Service.Code.Category=="Doctor Consult"
                         & NHGPFY18_clean3$NHGP.NUP.ACWO=="Well")

overlap_well_nursing_acute <- subset(NHGPFY18_clean3, NHGPFY18_clean3$NHGP.NUP.Service.Code.Category=="Nursing"
                                     & NHGPFY18_clean3$NHGP.NUP.ACWO=="Well"
                                     & NHGPFY18_clean3$Visit.Category=="Acute")

overlap_well_nursing_chronic <- subset(NHGPFY18_clean3, NHGPFY18_clean3$NHGP.NUP.Service.Code.Category=="Nursing"
                                       & NHGPFY18_clean3$NHGP.NUP.ACWO=="Well"
                                       & NHGPFY18_clean3$Visit.Category=="Chronic")

overlap_well_nursing_others <- subset(NHGPFY18_clean3, NHGPFY18_clean3$NHGP.NUP.Service.Code.Category=="Nursing"
                                   & NHGPFY18_clean3$NHGP.NUP.ACWO=="Well"
                                   & NHGPFY18_clean3$Visit.Category=="Others")
