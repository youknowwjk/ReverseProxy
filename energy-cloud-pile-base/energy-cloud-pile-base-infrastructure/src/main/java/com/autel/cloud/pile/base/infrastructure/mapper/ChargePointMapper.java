//package com.autel.cloud.pile.base.infrastructure.mapper;
//
//import com.autel.cloud.pile.base.dto.SubscriptionStatusCountDTO;
//import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargePointEntity;
//import com.baomidou.mybatisplus.core.mapper.BaseMapper;
//import org.apache.ibatis.annotations.Param;
//
//import java.util.List;
//
//public interface ChargePointMapper extends BaseMapper<ChargePointEntity> {
//    List<SubscriptionStatusCountDTO> subscriptionStatusCountByOwner(@Param("merchantId") Long merchantId);
//
//    List<SubscriptionStatusCountDTO> subscriptionStatusCountByMaintenance(Long merchantId);
//
//    List<SubscriptionStatusCountDTO> subscriptionStatusCountByDealer(Long merchantId);
//}
