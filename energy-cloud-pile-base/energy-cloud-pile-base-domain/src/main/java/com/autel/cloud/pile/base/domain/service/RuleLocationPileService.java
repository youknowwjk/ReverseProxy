package com.autel.cloud.pile.base.domain.service;


import com.autel.cloud.pile.base.dto.GunInformationDTO;
import com.autel.cloud.pile.base.dto.PileDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.RuleLocationPileEntity;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
* @author temp
* @description 针对表【tb_rule_location_pile(进场控制规则与桩关系表)】的数据库操作Service
* @createDate 2022-07-07 21:19:47
*/
public interface RuleLocationPileService extends IService<RuleLocationPileEntity> {

    boolean batchInsert(Long ruleId, List<PileDTO> piles);

    boolean batchInsertEvse(Long ruleId, List<GunInformationDTO> piles);

    boolean deleteRuleRelateByLocationId(Long locationId);

    boolean deleteRuleRelateByPileId(Long pileId);

    List<RuleLocationPileEntity> findAll();

    List<String> selectBindEvse(List<String> snList);

    Integer syncRuleToEvse();
}
