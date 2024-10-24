package com.autel.cloud.pile.base.domain.convert.pile;

import com.autel.cloud.pile.base.vo.PileEvseVO;
import com.autel.cloud.pile.base.vo.PileVO;
import com.autel.cloud.pile.base.vo.evse.SaveEvseInfoVO;
import com.autel.cloud.pile.base.vo.pile.SavePileInfoVO;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

public class PileInfoConvert {

    private PileInfoConvert() {
    }

    public static List<SavePileInfoVO> buildSavePileInfoVOList(List<PileVO> pileVOList) {

        if (ObjectUtils.isEmpty(pileVOList)) {
            return null;
        }

        List<SavePileInfoVO> savePileInfoVOList = new ArrayList<>();
        pileVOList.stream()
                .filter(PileInfoConvert::correctPileVO)
                .forEach(var -> savePileInfoVOList.add(PileInfoConvert.buildSavePileInfoVO(var)));
        return savePileInfoVOList;
    }

    private static boolean correctPileVO(PileVO pileVO) {

        if (pileVO == null
                || StringUtils.isBlank(pileVO.getPileSN())
                || pileVO.getPileId() == null) {
            return false;
        }
        return PileInfoConvert.correctPileEvseVOList(pileVO.getPileEvseVOS());
    }

    private static boolean correctPileEvseVOList(List<PileEvseVO> pileEvseVOList) {
        if (ObjectUtils.isEmpty(pileEvseVOList)) {
            return false;
        }
        boolean flag = true;
        for (PileEvseVO pileEvseVO : pileEvseVOList) {
            if (!PileInfoConvert.correctPileEvseVO(pileEvseVO)) {
                flag = false;
                break;
            }
        }
        return flag;
    }

    private static boolean correctPileEvseVO(PileEvseVO pileEvseVO) {
        if (pileEvseVO == null) {
            return false;
        }
        return pileEvseVO.getEvseId() != null && StringUtils.isNotBlank(pileEvseVO.getConnector());
    }

    private static SavePileInfoVO buildSavePileInfoVO(PileVO pileVO) {
        SavePileInfoVO savePileInfoVO = new SavePileInfoVO();
        savePileInfoVO.setPileId(pileVO.getPileId());
        savePileInfoVO.setPileSn(pileVO.getPileSN());
        savePileInfoVO.setPileName(pileVO.getPileName());
        List<SaveEvseInfoVO> saveEvseInfoVOList = new ArrayList<>();
        pileVO.getPileEvseVOS().forEach(var -> {
            SaveEvseInfoVO saveEvseInfoVO = new SaveEvseInfoVO();
            saveEvseInfoVO.setEvseId(var.getEvseId());
            saveEvseInfoVO.setConnector("0" + var.getConnector());
            saveEvseInfoVOList.add(saveEvseInfoVO);
        });
        savePileInfoVO.setSaveEvseInfoVOList(saveEvseInfoVOList);
        return savePileInfoVO;
    }
}
