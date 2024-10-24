package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.dto.TimePowerDTO;
import com.autel.cloud.pile.base.vo.OpLocationEvseStateCountVO;
import com.autel.cloud.pile.bill.dto.ChargePowerDTO;
import com.autel.cloud.pile.bill.dto.EmsGroupPowerDTO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;

import java.util.List;

/**
 * @Author A22282
 * @Date 2024/1/23 10:44
 */
public interface HomeService {
    Page<ChargePowerDTO> queryTimePower(TimePowerDTO dto);

    EmsGroupPowerDTO queryEmsTimePower(Long groupId);

    List<OpLocationEvseStateCountVO> queryEmsEvseStateCount(Long groupId);
}
