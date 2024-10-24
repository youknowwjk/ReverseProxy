package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.pile.base.VipPolicy;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileVipConfigEntity;
import com.autel.cloud.pile.user.api.dto.MemberCleanDTO;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import java.util.List;

public interface OpLocationPileVipConfigRepository {

//    Date LONG_TERM_EXPIRE_DATE = new Date(LocalDateTime.parse("9999-12-31 23:59:59", DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")).atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());

//    Date LONG_TERM_EFFECTIVE_DATE = new Date(LocalDateTime.parse("2023-01-01 00:00:00", DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")).atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());

    LocalDateTime LONG_TERM_EXPIRE_DATE = LocalDateTime.parse("9999-12-31 23:59:59", DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));

    LocalDateTime LONG_TERM_EFFECTIVE_DATE = LocalDateTime.parse("2023-01-01 00:00:00", DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
    /**
     * @param dto 保存集合
     */
    void save(VipPolicy dto);

    /**
     * @param list 保存集合
     */
    void save(List<VipPolicy> list);

    void batchAddOrUpdate(List<VipPolicy> list);

    /**
     * 根据智能充电分组配置ID 查询VIP
     *
     * @param groupId 智能充电分组ID
     * @return VIP
     */
    List<VipPolicy> findByGroupId(Long groupId);


    List<VipPolicy> findList(Long groupId);

    /**
     * 根据智能充电分组配置ID 查询VIP客户
     *
     * @param groupId 智能充电分组ID
     * @return 客户IDs
     */
    List<Long> findMemberIdsByGroupId(Long groupId);

    int deleteByGroupId(Long groupId);

    int deleteByGroupIds(Collection<Long> groupIds);

    List<OpLocationPileVipConfigEntity> findByTypeAndPrincipalId(int type, Long principalId);

    Integer removeMembers(MemberCleanDTO dto);

    Integer syncMemberGroupId();

    List<OpLocationPileVipConfigEntity> findUse();
}
