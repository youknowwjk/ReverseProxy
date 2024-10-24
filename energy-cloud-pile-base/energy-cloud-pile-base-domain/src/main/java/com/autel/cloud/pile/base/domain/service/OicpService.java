package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.pile.base.dto.oicp.EroamingEvseData;
import com.autel.cloud.pile.base.vo.EvseDynamicPricingVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;

/**
 * @author X21216
 * @date 2023/3/23  17:59
 */
public interface OicpService {

    Page<EroamingEvseData> queryEvseDate(Integer page, Integer pageSize);

    /**
     * @param pageDTO
     * @return
     * @function 提供清单页面供EMP拉取(OICP Hubject)
     */
    Page<EvseDynamicPricingVO> page(PageDTO pageDTO);
}
