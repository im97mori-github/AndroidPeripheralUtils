package org.im97mori.ble.android.peripheral.ui.device.setting.u2a23;

import static org.im97mori.ble.android.peripheral.utils.Utils.setTextDistinct;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.view.MenuProvider;

import androidx.lifecycle.HasDefaultViewModelProviderFactory;
import androidx.lifecycle.ViewModelProvider;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.databinding.SystemIdSettingActivityBinding;
import org.im97mori.ble.android.peripheral.utils.AfterTextChangedTextWatcher;
import org.im97mori.ble.android.peripheral.utils.AutoDisposeViewModelProvider;
import org.im97mori.stacklog.LogUtils;

import dagger.hilt.android.AndroidEntryPoint;

import javax.inject.Inject;
import java.util.function.Function;

@AndroidEntryPoint
public class SystemIdSettingActivity extends AppCompatActivity {

    @Inject
    Function<HasDefaultViewModelProviderFactory, ViewModelProvider.Factory> viewModelProviderFactoryFunction;

    private SystemIdSettingViewModel mViewModel;

    private SystemIdSettingActivityBinding mBinding;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewModel = new AutoDisposeViewModelProvider(this, viewModelProviderFactoryFunction.apply(this)).get(SystemIdSettingViewModel.class);

        mBinding = SystemIdSettingActivityBinding.inflate(getLayoutInflater());
        setContentView(mBinding.getRoot());

        mViewModel.observeIsErrorResponse(this, check -> {
            mBinding.manufacturerIdentifier.setVisibility(check ? View.GONE : View.VISIBLE);
            mBinding.organizationallyUniqueIdentifier.setVisibility(check ? View.GONE : View.VISIBLE);
            mBinding.responseCode.setVisibility(check ? View.VISIBLE : View.GONE);
            mBinding.isErrorResponse.setChecked(check);
        });
        mBinding.isErrorResponse.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateIsErrorResponse(isChecked));

        mViewModel.observeManufacturerIdentifier(this, charSequence -> setTextDistinct(mBinding.manufacturerIdentifierEdit, charSequence));
        mViewModel.observeManufacturerIdentifierErrorString(this, mBinding.manufacturerIdentifier::setError);
        mBinding.manufacturerIdentifierEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateManufacturerIdentifier(editable)));

        mViewModel.observeOrganizationallyUniqueIdentifier(this, charSequence -> setTextDistinct(mBinding.organizationallyUniqueIdentifierEdit, charSequence));
        mViewModel.observeOrganizationallyUniqueIdentifierErrorString(this, mBinding.organizationallyUniqueIdentifier::setError);
        mBinding.organizationallyUniqueIdentifierEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateOrganizationallyUniqueIdentifier(editable)));

        mViewModel.observeResponseCode(this, charSequence -> setTextDistinct(mBinding.responseCodeEdit, charSequence));
        mViewModel.observeResponseCodeErrorString(this, mBinding.responseCode::setError);
        mBinding.responseCodeEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateResponseCode(editable)));

        mViewModel.observeResponseDelay(this, charSequence -> setTextDistinct(mBinding.responseDelayEdit, charSequence));
        mViewModel.observeResponseDelayErrorString(this, mBinding.responseDelay::setError);
        mBinding.responseDelayEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateResponseDelay(editable)));

        mViewModel.observeSavedData(this, intent -> {
            setResult(RESULT_OK, intent);
            finish();
        });

        mBinding.topAppBar.addMenuProvider(new MenuProvider() {

            @Override
            public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                menu.findItem(R.id.save).setEnabled(mBinding.rootContainer.getVisibility() == View.VISIBLE);
            }

            @Override
            public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                boolean result = false;
                if (menuItem.getItemId() == R.id.save) {
                    mViewModel.save(throwable
                            -> Toast.makeText(SystemIdSettingActivity.this
                            , throwable.getMessage()
                            , Toast.LENGTH_SHORT).show());
                    result = true;
                }
                return result;
            }
        });
    }

    @Override
    protected void onStart() {
        super.onStart();
        mViewModel.observeSetup(getIntent()
                , () -> {
                    mBinding.rootContainer.setVisibility(View.VISIBLE);
                    mBinding.topAppBar.invalidateMenu();
                }
                , throwable -> LogUtils.stackLog(throwable.getMessage()));
    }

}
