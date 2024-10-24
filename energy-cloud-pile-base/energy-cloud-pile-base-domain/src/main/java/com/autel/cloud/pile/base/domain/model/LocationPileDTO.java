package com.autel.cloud.pile.base.domain.model;

import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.vo.OpLocationPileEvseGroupListVO;
import com.autel.cloud.pile.base.vo.OpLocationPileGroupAssociateVO;
import lombok.Data;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Data
public class LocationPileDTO {

    private List<OpLocationPileEvseGroupListVO.PileInfoVo> pileInfoVoList = new ArrayList<>();

    private Map<Long, List<OpLocationPileGroupAssociateVO>> associateMap = new HashMap<>();

    private Map<String, OpLocationPileEvseElasticDTO> pileDtoMap = new HashMap<>();

    private Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap = new HashMap<>();
}
