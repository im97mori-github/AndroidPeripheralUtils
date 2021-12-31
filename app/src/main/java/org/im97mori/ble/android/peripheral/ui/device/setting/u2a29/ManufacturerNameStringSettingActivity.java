package org.im97mori.ble.android.peripheral.ui.device.setting.u2a29;

import static org.im97mori.ble.android.peripheral.utils.Utils.setTextDistinct;

import android.os.Bundle;
import android.view.MenuItem;
import android.view.View;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.databinding.ManufacturerNameStringSettingActivityBinding;
import org.im97mori.ble.android.peripheral.utils.AfterTextChangedTextWatcher;
import org.im97mori.ble.android.peripheral.utils.MockitoViewModelProvider;
import org.im97mori.stacklog.LogUtils;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class ManufacturerNameStringSettingActivity extends AppCompatActivity {

    private ManufacturerNameStringSettingViewModel mViewModel;

    private ManufacturerNameStringSettingActivityBinding mBinding;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewModel = new MockitoViewModelProvider(this).get(ManufacturerNameStringSettingViewModel.class);

        mBinding = ManufacturerNameStringSettingActivityBinding.inflate(getLayoutInflater());
        setContentView(mBinding.getRoot());

        mViewModel.observeIsErrorResponse(this, check -> {
            mBinding.manufacturerNameString.setVisibility(check ? View.GONE : View.VISIBLE);
            mBinding.responseCode.setVisibility(check ? View.VISIBLE : View.GONE);
            mBinding.isErrorResponse.setChecked(check);
        });
        mBinding.isErrorResponse.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateIsErrorResponse(isChecked));

        mViewModel.observeManufacturerNameString(this, charSequence -> setTextDistinct(mBinding.manufacturerNameStringEdit, charSequence));
        mViewModel.observeManufacturerNameStringError(this, mBinding.manufacturerNameString::setError);
        mBinding.manufacturerNameStringEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateManufacturerNameString(editable)));

        mViewModel.observeResponseCode(this, charSequence -> setTextDistinct(mBinding.responseCodeEdit, charSequence));
        mViewModel.observeResponseCodeError(this, mBinding.responseCode::setError);
        mBinding.responseCodeEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateResponseCode(editable)));

        mViewModel.observeResponseDelay(this, charSequence -> setTextDistinct(mBinding.responseDelayEdit, charSequence));
        mViewModel.observeResponseDelayError(this, mBinding.responseDelay::setError);
        mBinding.responseDelayEdit.addTextChangedListener(new AfterTextChangedTextWatcher(editable
                -> mViewModel.updateResponseDelay(editable)));

        mBinding.topAppBar.setOnMenuItemClickListener(this::onOptionsItemSelected);
    }

    @Override
    protected void onStart() {
        super.onStart();
        mViewModel.observeSetup(getIntent()
                , () -> mBinding.rootContainer.setVisibility(View.VISIBLE)
                , throwable -> LogUtils.stackLog(throwable.getMessage()));
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        boolean result;
        if (item.getItemId() == R.id.save) {
            mViewModel.observeSave(intent -> {
                setResult(RESULT_OK, intent);
                finish();
            }, throwable -> Toast.makeText(this, throwable.getMessage(), Toast.LENGTH_SHORT).show());
            result = true;
        } else {
            result = super.onOptionsItemSelected(item);
        }
        return result;
    }

}
