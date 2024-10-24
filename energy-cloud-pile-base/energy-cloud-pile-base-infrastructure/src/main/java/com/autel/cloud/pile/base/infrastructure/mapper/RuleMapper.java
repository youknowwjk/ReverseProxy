package com.autel.cloud.pile.base.infrastructure.mapper;


import com.autel.cloud.pile.base.dto.RuleLocationPileDTO;
import com.autel.cloud.pile.base.dto.SerachBindEvseDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.RuleEntity;
import com.autel.cloud.pile.base.vo.RuleGroupVO;
import com.autel.cloud.pile.base.vo.RuleVO;
import com.autel.cloud.pile.base.vo.SerachBindEvseVO;
import com.autel.cloud.pile.base.vo.batch.BindRuleEvseVo;
import com.autel.cloud.pile.base.vo.batch.EvseRuleVO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * @author temp
 * @description 针对表【tb_rule(进场控制规则表)】的数据库操作Mapper
 * @createDate 2022-07-06 11:40:00
 * @Entity generator.domain.RuleEntity
 */
@Mapper
@Repository
public interface RuleMapper extends BaseMapper<RuleEntity> {

    List<RuleLocationPileDTO> findRuleByLocationIds(@Param("locationIds") List<Long> locationIds, @Param("keyword") String keyword, @Param("ruleId") Long ruleId);
    List<RuleVO> findRulesBySellerId(@Param("sellerId") Long sellerId, @Param("keyword") String keyword);

    String getNameByLocationIdAndPileId(@Param("locationId") Long locationId, @Param("pileId") Long pileId);

    RuleVO findById(@Param("ruleId") Long ruleId);

    List<RuleVO> findRelateCount(@Param("pileIds") List<Long> pileIds);

    List<RuleVO> findCountByRuleIds(@Param("evseSnList") List<String> evseSnList, @Param("sellerId")Long sellerId);

    List<SerachBindEvseVO> findListByEvseSn(@Param("evseSnList") List<String> evseSnList, @Param("sellerId") Long sellerId);

    IPage<SerachBindEvseVO> findEvseBySellerId(IPage<SerachBindEvseVO> page, @Param("dto") SerachBindEvseDTO serachBindEvseDTO,@Param("sellerId") Long sellerId);

    IPage<RuleVO> searchPage(IPage<RuleVO> resultPage, @Param("dto")SerachBindEvseDTO serachBindEvseDTO,@Param("sellerId") Long sellerId);

    List<RuleGroupVO> getRuleNameByGroupId(@Param("groupIds") List<String> groupIds, @Param("sellerId") Long sellerId);

    List<SerachBindEvseVO> findBindEvseBySellerId(@Param("sellerId") Long sellerId);

    List<EvseRuleVO> findRulesList(@Param("sellerId") Long sellerId);

    List<BindRuleEvseVo> findBindEvseAndRule(@Param("evseSnList") List<String> evseSnList, @Param("sellerId")Long sellerId);

}




