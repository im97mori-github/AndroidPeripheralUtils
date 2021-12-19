package org.im97mori.ble.android.peripheral.ui.device.setting.u2902;

import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.MenuItem;
import android.view.View;
import android.widget.CheckBox;
import android.widget.RadioButton;
import android.widget.RadioGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.appbar.MaterialToolbar;
import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.TestApplication;
import org.im97mori.ble.android.peripheral.ui.BaseActivity;

import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class ClientCharacteristicConfigurationSettingActivity extends BaseActivity {

    private ClientCharacteristicConfigurationSettingViewModel mViewModel;

    private CheckBox mErrorResponseCodeCheckBox;

    private RadioGroup mPropertiesRadioGroup;
    private RadioButton mPropertiesDisabledRadioButton;
    private RadioButton mPropertiesEnabledRadioButton;
    private TextInputLayout mErrorResponseCode;
    private TextInputEditText mErrorResponseCodeEdit;
    private TextInputLayout mResponseDelay;
    private TextInputEditText mResponseDelayEdit;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        mApplicationComponent = ((TestApplication) getApplication()).getComponent();

        mApplicationComponent.inject(this);
        super.onCreate(savedInstanceState);
        mViewModel = new ViewModelProvider(this).get(ClientCharacteristicConfigurationSettingViewModel.class);
        mApplicationComponent.inject(mViewModel);

        setContentView(R.layout.client_characteristic_configuration_setting_activity);

        mErrorResponseCodeCheckBox = findViewById(R.id.errorResponseCheckBox);

        mPropertiesRadioGroup = findViewById(R.id.propertiesRadioGroup);
        mPropertiesDisabledRadioButton = findViewById(R.id.propertiesDisabledRadioButton);
        mPropertiesEnabledRadioButton = findViewById(R.id.propertiesEnabledRadioButton);

        mResponseDelay = findViewById(R.id.responseDelay);
        mResponseDelayEdit = (TextInputEditText) mResponseDelay.getEditText();
        mErrorResponseCode = findViewById(R.id.errorResponseCode);
        mErrorResponseCodeEdit = (TextInputEditText) mErrorResponseCode.getEditText();

        mViewModel.observeIsErrorResponse(this, aBoolean -> {
            mErrorResponseCode.setVisibility(aBoolean ? View.VISIBLE : View.GONE);
            mErrorResponseCodeCheckBox.setChecked(aBoolean);
        });
        mErrorResponseCodeCheckBox.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateIsErrorResponse(isChecked));

        mViewModel.observeResponseCode(this, charSequence -> distinctSetText(mErrorResponseCodeEdit, charSequence));
        mViewModel.observeResponseCodeError(this, charSequence -> mErrorResponseCode.setError(charSequence));
        mErrorResponseCodeEdit.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mViewModel.updateResponseCode(s);
            }
        });

        mViewModel.observeResponseDelayError(this, charSequence -> mResponseDelay.setError(charSequence));
        mViewModel.observeResponseDelay(this, charSequence -> distinctSetText(mResponseDelayEdit, charSequence));
        mResponseDelayEdit.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mViewModel.updateResponseDelay(s);
            }
        });

        mViewModel.observeProperties(this, aBoolean -> mPropertiesRadioGroup.check(aBoolean ? R.id.propertiesEnabledRadioButton : R.id.propertiesDisabledRadioButton));
        mPropertiesRadioGroup.setOnCheckedChangeListener((group, checkedId) -> mViewModel.updateProperties(R.id.propertiesEnabledRadioButton == checkedId));

        mViewModel.observeDisabledProperties(this, charSequence -> mPropertiesDisabledRadioButton.setText(charSequence));
        mViewModel.observeEnabledProperties(this, charSequence -> mPropertiesEnabledRadioButton.setText(charSequence));

        MaterialToolbar bar = findViewById(R.id.topAppBar);
        bar.setOnMenuItemClickListener(this::onOptionsItemSelected);
    }

    @Override
    protected void onStart() {
        super.onStart();
        mDisposable.add(mViewModel.setup(getIntent())
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(() -> findViewById(R.id.rootContainer).setVisibility(View.VISIBLE)));
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        boolean result = false;
        if (item.getItemId() == R.id.save) {
            mDisposable.add(mViewModel.save()
                    .subscribeOn(Schedulers.io())
                    .observeOn(AndroidSchedulers.mainThread())
                    .subscribe(intent -> {
                                if (intent.isPresent()) {
                                    setResult(RESULT_OK, intent.get());
                                    finish();
                                }
                            }
                    ));
        } else {
            result = super.onOptionsItemSelected(item);
        }
        return result;
    }

}
