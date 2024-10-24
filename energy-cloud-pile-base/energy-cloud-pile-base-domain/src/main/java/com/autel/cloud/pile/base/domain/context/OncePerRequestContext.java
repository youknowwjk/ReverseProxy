package com.autel.cloud.pile.base.domain.context;

import com.autel.cloud.pile.base.dto.DeliveryByUserDTO;
import com.autel.cloud.pile.base.dto.DeliveryGroupDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupAssociateEntity;
import com.autel.cloud.pile.base.vo.DeliveryGroupTreeVO;
import com.autel.cloud.pile.base.vo.OpLocationPileGroupDeliveryVO;
import com.autel.cloud.smart.monitor.dto.EvseMonitorMistakeDTO;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 智能充电请求上下文
 *
 * @Author temp
 * @Date 2023/8/31 11:32
 * @Since 1.0.0
 */
@Slf4j
@Data
public class OncePerRequestContext {

    /**
     * 根群组ID
     */
    private final Long rootId;

    /**
     * 触发源
     * 1：添加群组
     * 2：更新群组
     * 3：状态变更
     * 4：监控调整
     * 5：删除群组
     * 6：群组禁用
     * 7：删除桩
     * 8：ALM
     * 9：暂停/优先充电
     * 10：刷新结束时间
     * 11：先降后升
     * 12：分时设置
     * 13：用户设置
     */
    private final Integer source;


    private Boolean deliveryFlag = true;

    /**
     * 群组列表
     */
    private List<DeliveryGroupDTO> deliveryGroupList;

    /**
     * pileSn列表
     */
    private List<String> pileSnList;

    /**
     * 根群组
     */
    private DeliveryGroupDTO rootDto;

    /**
     * 时区ID
     */
    private String zoneId;

    /**
     * 群组是否启用
     */
    private Integer status = 1;

    /**
     * 树形结构
     */
    private DeliveryGroupTreeVO deliveryTreeVo;

    /**
     * 用户特征
     */
    private Map<String, DeliveryByUserDTO> userDTOMap = new HashMap<>();

    /**
     * 指定值下发
     */
    private Map<String, BigDecimal> designatedMap = new HashMap<>();

    /**
     * 监控指定值
     */
    private Map<String, EvseMonitorMistakeDTO> monitorDtoMap = new HashMap<>();

    /**
     * 分配结果
     */
    private List<OpLocationPileGroupDeliveryVO> deliveryList;

    /**
     * 删除列表
     */
    private List<OpLocationPileGroupAssociateEntity> deleteList;

    public OncePerRequestContext(Long rootId, Integer source) {
        this.rootId = rootId;
        this.source = source;
        this.init();
    }

    public Boolean put(EvseMonitorMistakeDTO dto) {
        return monitorDtoMap.put(dto.getEvseSn(), dto) != null;
    }

    public Boolean putAll(Collection<EvseMonitorMistakeDTO> list) {
        if (CollectionUtils.isEmpty(list)) {
            return false;
        }
        list.stream().forEach(dto -> {
            this.put(dto);
        });
        return true;
    }

    private void init() {

    }

    /**
     * 按指定值下发
     *
     * @param evseSn
     * @param value
     * @return
     */
    public Boolean add(String evseSn, BigDecimal value) {
        return this.designatedMap.put(evseSn, value) != null;
    }

    /**
     * 按指定值下发
     *
     * @param map
     */
    public void addAll(Map<String, BigDecimal> map) {
        this.designatedMap.putAll(map);
    }
}
