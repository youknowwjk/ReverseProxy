package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.dto.ChargePointFuzzQueryDTO;
import com.autel.cloud.pile.base.dto.pile.QueryPileListDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargePointMerchantRelationEntity;
import com.autel.cloud.pile.base.vo.ChargePointAssetVO;
import com.autel.cloud.pile.base.vo.PileBaseVO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.annotations.Param;

import java.util.List;

public interface ChargePointMerchantRelationMapper extends BaseMapper<ChargePointMerchantRelationEntity> {


    List<String> existsPowerTypes(@Param("merchantId") Long  merchantId);

    String getConnectors(@Param("sn") String sn);

    Page<ChargePointAssetVO> getPage(Page<ChargePointAssetVO> page,
                                     @Param("params") ChargePointFuzzQueryDTO chargePointQueryDTO,
                                     @Param("merchantId") Long merchantId);

    Page<ChargePointMerchantRelationEntity> selectPageByCustomer(Page<ChargePointMerchantRelationEntity> pageInfo, @Param("params") ChargePointFuzzQueryDTO chargePointQueryDTO);

    Page<ChargePointMerchantRelationEntity> selectByOrder(Page<ChargePointMerchantRelationEntity> pageInfo, @Param("params") ChargePointFuzzQueryDTO chargePointQueryDTO);

    List<PileBaseVO> queryPileBySn(@Param("paramDTO") QueryPileListDTO paramDTO);

}
