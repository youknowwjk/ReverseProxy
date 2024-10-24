package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.infrastructure.sysconfig.log.dto.UserOpLogDTO;

/**
 * @author A22203
 * @Description
 * @Date 2022/5/17 19:51
 */
public interface UserOpLogRepository {

    PageVO<UserOpLogDTO> selectUserOpLogPage(UserOpLogDTO dto);

    Integer save(UserOpLogDTO userOpLogDto);
}
