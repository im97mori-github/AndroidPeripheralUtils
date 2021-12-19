package org.im97mori.ble.android.peripheral.ui.device.setting;

import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.MenuItem;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.fragment.app.FragmentTransaction;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.appbar.MaterialToolbar;
import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.TestApplication;
import org.im97mori.ble.android.peripheral.ui.BaseActivity;

public class DeviceSettingActivity extends BaseActivity {

    private DeviceSettingViewModel mViewModel;

    private MaterialToolbar mMaterialToolbar;

    private TextInputLayout mDeviceName;
    private TextInputEditText mDeviceNameEdit;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        mApplicationComponent = ((TestApplication) getApplication()).getComponent();

        mApplicationComponent.inject(this);
        super.onCreate(savedInstanceState);
        mViewModel = new ViewModelProvider(this).get(DeviceSettingViewModel.class);
        mApplicationComponent.inject(mViewModel);

        setContentView(R.layout.device_setting_activity);

        mDeviceName = findViewById(R.id.deviceName);
        mDeviceNameEdit = (TextInputEditText) mDeviceName.getEditText();

        mViewModel.observeDeviceNameError(this, s -> mDeviceName.setError(s));
        mDeviceNameEdit.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mViewModel.updateDeviceNameEdit(s.toString());
            }
        });
        mViewModel.observeDeviceNameEdit(this
                , s -> distinctSetText(mDeviceNameEdit, s));

        mMaterialToolbar = findViewById(R.id.topAppBar);
        mMaterialToolbar.setOnMenuItemClickListener(this::onOptionsItemSelected);
        mViewModel.observeTitle(this, s -> mMaterialToolbar.setTitle(s));

        if (getSupportFragmentManager().findFragmentById(R.id.container) == null) {
            FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
            transaction.add(R.id.container, mViewModel.getFragment(getIntent()));
            transaction.commitNow();
        }
    }

    @Override
    protected void onStart() {
        super.onStart();
        mDisposable.add(mViewModel.setup(getIntent())
                .subscribe(() -> findViewById(R.id.rootContainer).setVisibility(View.VISIBLE)
                        , Throwable::printStackTrace));
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        boolean result = false;
        if (item.getItemId() == R.id.save) {
            mDisposable.add(mViewModel.save()
                    .subscribe(() -> {
                        setResult(RESULT_OK);
                        finish();
                    }, Throwable::printStackTrace));
        } else {
            result = super.onOptionsItemSelected(item);
        }
        return result;
    }

}