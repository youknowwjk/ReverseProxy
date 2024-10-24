package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.pile.base.dto.MailboxConfigDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbMailboxConfigEntity;
import com.baomidou.mybatisplus.extension.service.IService;

public interface MailboxConfigRepository extends IService<TbMailboxConfigEntity> {

    /**
     * 新增邮箱
     * @param mailboxConfigDTO
     * @return
     */
    Boolean addMailbox(MailboxConfigDTO mailboxConfigDTO);

    /**
     * 修改邮箱
     * @param mailboxConfigDTO
     * @return
     */
    Boolean updateMailbox(MailboxConfigDTO mailboxConfigDTO);

    /**
     * 删除邮箱
     * @param id
     * @return
     */
    Boolean delMailbox(Long id);

    /**
     * 分页查询邮箱
     * @param mailboxConfigDTO
     * @return
     */
    PageVO<MailboxConfigDTO> selectMailboxPage(MailboxConfigDTO mailboxConfigDTO);

    /**
     * 根据商户ID查询邮箱详情
     * @param operatorId
     * @return
     */
    TbMailboxConfigEntity selectMailboxByOperatorId(Long operatorId);
}
